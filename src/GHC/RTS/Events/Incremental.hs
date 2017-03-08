{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module GHC.RTS.Events.Incremental
  ( -- * Incremental API
    Decoder
  , decodeHeader
  , decodeEvents
  , decodeEventLog

  -- * Lazy API
  , readHeader
  , readEvents
  , readEventLog

  -- * IO interface
  , readEventLogFromFile
  , printEventsIncremental
  , hPrintEventsIncremental
  ) where
import Control.Monad
import Data.Monoid
import Data.Word
import System.IO

import qualified Data.Binary.Get as G
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Data.IntMap.Strict as IM

import GHC.RTS.EventParserUtils
import GHC.RTS.EventTypes
import GHC.RTS.Events

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

data Decoder a
  = Consume (B.ByteString -> Decoder a)
  | Produce !a (Decoder a)
  | Done B.ByteString
  | Error B.ByteString String

pushChunk :: Decoder a -> B.ByteString -> Decoder a
pushChunk decoder chunk = case decoder of
  Consume k -> k chunk
  Produce a decoder' -> decoder `pushChunk` chunk
  Done leftover -> Done $ leftover `B.append` chunk
  Error leftover err -> Error (leftover `B.append` chunk) err

withHeader :: (Header -> B.ByteString -> Decoder r) -> Decoder r
withHeader f = go $ G.runGetIncremental getHeader
  where
    go decoder = case decoder of
      G.Done leftover _ header -> f header leftover
      G.Partial k -> Consume $ \chunk -> go $ k $ Just chunk
      G.Fail leftover _ err -> Error leftover err

decodeHeader :: Decoder Header
decodeHeader = withHeader $ \header leftover -> Produce header $ Done leftover

decodeEvents :: Header -> Decoder Event
decodeEvents header = go (0 :: Int) Nothing decoder0
  where
    decoder0 = mkEventDecoder header
    go !remaining !blockCap decoder = case decoder of
      G.Done leftover consumed r -> do
        let !decoder' = decoder0 `G.pushChunk` leftover
        case r of
          Just event -> case evSpec event of
            EventBlock {..} ->
              go (fromIntegral block_size) (mkCap cap) decoder'
            _ -> do
              let
                !remaining' = remaining - fromIntegral consumed
                !blockCap' = if remaining' > 0 then blockCap else Nothing
                !event' = event { evCap = blockCap }
              Produce event' $ go remaining' blockCap' decoder'
          Nothing -> go remaining blockCap decoder'
      G.Partial k ->
        Consume $ \chunk -> go remaining blockCap $ k $ Just chunk
      G.Fail leftover _ err ->
        Error leftover err
    mkCap cap = do
      guard $ fromIntegral cap /= (-1 :: Word16)
      return cap

decodeEventLog :: Decoder Event
decodeEventLog = withHeader $ \header leftover ->
  decodeEvents header `pushChunk` leftover

readHeader :: BL.ByteString -> Either String (Header, BL.ByteString)
readHeader = go $ Left decodeHeader
  where
    go r bytes = case r of
      Left decoder -> case decoder of
        Produce header decoder' -> case decoder' of
          Done leftover -> Right (header, BL.Chunk leftover bytes)
          _ -> fail "readHeader: unexpected decoder"
        Consume k -> case bytes of
          BL.Empty -> fail "readHeader: not enough bytes"
          BL.Chunk chunk chunks -> go (Left $! k chunk) chunks
        Done _ -> fail "readHeader: unexpected termination"
        Error _ err -> fail err
      Right header -> Right (header, bytes)

readEvents :: Header -> BL.ByteString -> Either String [Event]
readEvents header = sequenceA . go (decodeEvents header)
  where
    go :: Decoder Event -> BL.ByteString -> [Either String Event]
    go decoder bytes = case decoder of
      Produce event decoder' -> Right event : go decoder' bytes
      Consume k -> case bytes of
        BL.Empty -> []
        BL.Chunk chunk chunks -> go (k chunk) chunks
      Done {} -> []
      Error _ err -> [Left err]

readEventLog :: BL.ByteString -> Either String EventLog
readEventLog bytes = do
  (header, bytes') <- readHeader bytes
  events <- readEvents header bytes'
  return $ EventLog header $ Data events

readEventLogFromFile :: FilePath -> IO (Either String EventLog)
readEventLogFromFile path = readEventLog <$> BL.readFile path

printEventsIncremental :: FilePath -> IO ()
printEventsIncremental path = withFile path ReadMode hPrintEventsIncremental

hPrintEventsIncremental :: Handle -> IO ()
hPrintEventsIncremental hdl = go decodeEventLog
  where
    go decoder = case decoder of
      Produce event decoder' -> do
        BB.hPutBuilder stdout $ buildEvent event <> "\n"
        go decoder'
      Consume k -> do
        chunk <- B.hGetSome hdl 4096
        unless (B.null chunk) $ go $ k chunk
      Done {} -> return ()
      Error _ err -> fail err

-- | Makes a decoder with all the required parsers when given a Header
mkEventDecoder :: Header -> G.Decoder (Maybe Event)
mkEventDecoder header = G.runGetIncremental $ getEvent parsers
  where
    imap = IM.fromList [(fromIntegral (num t), t) | t <- eventTypes header]
    -- This test is complete, no-one has extended this event yet and all future
    -- extensions will use newly allocated event IDs.
    is_ghc_6 = Just sz_old_tid == do
      create_et <- IM.lookup EVENT_CREATE_THREAD imap
      size create_et
    -- GHC6 writes an invalid header, we handle it here by using a
    -- different set of event parsers.  Note that the ghc7 event parsers
    -- are standard events, and can be used by other runtime systems that
    -- make use of threadscope.

    -- GHC-7.8.2 uses a different thread block status encoding,
    -- and therefore requires a different parser for the stop
    -- event. Later, in GHC-7.8.3, the old encoding was restored.
    -- GHC-7.8.2 can be recognised by presence and absence of
    -- events in the header:
    --   * User markers were added in GHC-7.8
    --   * an empty event HACK_BUG_T9003 was added in GHC-7.8.3
    -- This fix breaks software which uses ghc-events and combines
    -- user markers with the older stop status encoding. We don't
    -- know of any such software, though.
    is_pre77 = IM.notMember EVENT_USER_MARKER imap
    is_ghc782 = IM.member EVENT_USER_MARKER imap
      && IM.notMember EVENT_HACK_BUG_T9003 imap

    stopParsers
      | is_pre77 = pre77StopParsers
      | is_ghc782 = [ghc782StopParser]
      | otherwise = [post782StopParser]

    event_parsers
      | is_ghc_6 = concat
        [ standardParsers
        , ghc6Parsers
        , parRTSParsers sz_old_tid
        ]
      | otherwise = concat
        [ standardParsers
        , ghc7Parsers
        , stopParsers
        , parRTSParsers sz_tid
        , mercuryParsers
        , perfParsers
        ]
    parsers = EventParsers $ mkEventTypeParsers imap event_parsers
