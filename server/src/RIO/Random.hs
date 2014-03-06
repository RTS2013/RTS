{-# LANGUAGE Trustworthy #-}

module RIO.Random
( getRIORandom
, getRIOGen
, setRIOGen
, newRIOGen
, module Control.Monad.Random
, module Control.Monad.Trans.Class
) where

import qualified System.Random as R
import RIO.RIO
import RIO.Privileges
import Control.Monad.Random
import Control.Monad.Trans.Class

getRIORandom :: (R.StdGen -> (a, R.StdGen)) -> RIO ReadWrite a
getRIORandom = RIO . R.getStdRandom

getRIOGen :: RIO ReadWrite R.StdGen
getRIOGen = RIO R.getStdGen

setRIOGen :: R.StdGen -> RIO ReadWrite ()
setRIOGen = RIO . R.setStdGen

newRIOGen :: RIO ReadWrite R.StdGen
newRIOGen = RIO R.newStdGen