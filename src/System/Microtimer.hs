{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : System.Microtimer
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
--               (c) 2013 Austin Seipp
-- License     : BSD-style
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : GHC
--
-- A tiny module for measuring the time taken by an 'IO' action.
--
-- This module is a almost a direct copy of the
-- @Criterion.Measurement@ module from the @criterion@ package,
-- written by Bryan O'Sullivan.
-- 
module System.Microtimer
    ( -- * Running 'IO' actions.
      time          -- :: IO a -> IO (Double, a)
    , time_         -- :: IO a -> IO Double
      -- * Formatting timing results.
    , formatSeconds -- :: Double -> String
    ) where
import Control.Monad (liftM)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Text.Printf (printf)

-- | Time an 'IO' action and return the time taken for execution,
-- as well as the return value.
time :: IO a -> IO (Double, a)
time act = do
  start <- getTime
  result <- act
  end <- getTime
  let !delta = end - start
  return (delta, result)

-- | Time an 'IO' action, throwing away the result and returning
-- the time taken for execution.
time_ :: IO a -> IO Double
time_ act = fst `liftM` (time act)

-- | Convert a 'Double' value into a 'String' which specifies
-- how long something took in seconds.
formatSeconds :: Double -> String
formatSeconds k
    | k < 0      = '-' : formatSeconds (-k)
    | k >= 1     = k        `with` "s"
    | k >= 1e-3  = (k*1e3)  `with` "ms"
    | k >= 1e-6  = (k*1e6)  `with` "us"
    | k >= 1e-9  = (k*1e9)  `with` "ns"
    | k >= 1e-12 = (k*1e12) `with` "ps"
    | otherwise  = printf "%g s" k
     where with (t :: Double) (u :: String)
               | t >= 1e9  = printf "%.4g %s" t u
               | t >= 1e6  = printf "%.0f %s" t u
               | t >= 1e5  = printf "%.1f %s" t u
               | t >= 1e4  = printf "%.2f %s" t u
               | t >= 1e3  = printf "%.3f %s" t u
               | t >= 1e2  = printf "%.4f %s" t u
               | t >= 1e1  = printf "%.5f %s" t u
               | otherwise = printf "%.6f %s" t u

--------------------------------------------------------------------------------
-- Miscellaneous

getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime

{-- TODO: perhaps export this one day
runForAtLeast :: Double -> Int -> (Int -> IO a) -> IO (Double, Int, a)
runForAtLeast howLong initSeed act = loop initSeed (0::Int) =<< getTime
  where
    loop !seed !iters initTime = do
      now <- getTime
      when (now - initTime > howLong * 10) $
        fail (printf "took too long to run: seed %d, iters %d" seed iters)
      (elapsed,result) <- time (act seed)
      if elapsed < howLong
        then loop (seed * 2) (iters+1) initTime
        else return (elapsed, seed, result)
--}

