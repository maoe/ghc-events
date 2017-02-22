{-# LANGUAGE ViewPatterns #-}
module GHC.RTS.Events.Analysis.Timing
  ( durationMachine
  , intervalMachine
  ) where
import GHC.TypeLits (KnownNat)

import Data.TDigest (TDigest)
import Data.IntMap (IntMap)
import qualified Data.TDigest as TDigest
import qualified Data.IntMap as IntMap

import GHC.RTS.Events
import GHC.RTS.Events.Analysis

-- | Calculate event duration distribution per core
durationMachine
  :: KnownNat compression
  => (EventInfo -> Bool) -- ^ Start event
  -> (EventInfo -> Bool) -- ^ End event
  -> Machine (IntMap (Timestamp, TDigest compression)) Event
durationMachine isStart isEnd = Machine
  { initial = IntMap.empty
  , final = const False
  , alpha
  , delta
  }
  where
    alpha (evSpec -> info) = isStart info || isEnd info
    delta digests Event {..}
      | isStart evSpec = do
        cap <- evCap
        return $! IntMap.alter start cap digests
      | isEnd evSpec = do
        cap <- evCap
        return $! IntMap.alter end cap digests
      | otherwise = Nothing
      where
        start (Just (_, digest)) = Just (evTime, digest)
        start _ = Just (evTime, mempty)
        end (Just (startTime, digest)) = do
          let !pauseTime = fromIntegral $ evTime - startTime
              !digest' = TDigest.insert pauseTime digest
          return (startTime {- doesn't matter -}, digest')
        end _ = Nothing

-- | Calculate event interval distribution per core
intervalMachine
  :: KnownNat compression
  => (EventInfo -> Bool)
  -> Machine (IntMap (Timestamp, TDigest compression)) Event
intervalMachine match = Machine
  { initial = IntMap.empty
  , final = const False
  , alpha
  , delta
  }
  where
    alpha evt = match $ evSpec evt
    delta digests Event {..}
      | match evSpec = do
        cap <- evCap
        return $! IntMap.alter start cap digests
      | otherwise = Nothing
      where
        start Nothing = Just (evTime, mempty)
        start (Just (prevTime, digest)) = do
          let !interval = fromIntegral $ evTime - prevTime
              !digest' = TDigest.insert interval digest
          return (evTime, digest')
