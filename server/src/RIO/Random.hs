{-# LANGUAGE Trustworthy #-}

module RIO.Random
( getStdRandom
, getStdGen
, setStdGen
, newStdGen
) where

import qualified System.Random as R
import RIO.RIO
import RIO.Prelude

getStdRandom :: (R.StdGen -> (a, R.StdGen)) -> RIO ReadWrite a
getStdRandom = RIO . R.getStdRandom

getStdGen :: RIO ReadWrite R.StdGen
getStdGen = RIO R.getStdGen

setStdGen :: R.StdGen -> RIO ReadWrite ()
setStdGen = RIO . R.setStdGen

newStdGen :: RIO ReadWrite R.StdGen
newStdGen = RIO R.newStdGen