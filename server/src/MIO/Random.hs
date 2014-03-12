{-# LANGUAGE Trustworthy #-}

module MIO.Random
( Read(..)
, Write(..)
, module Control.Monad.Random
, module Control.Monad.Trans.Class
) where

import qualified System.Random as R
import MIO.MIO
import MIO.Privileges ()
import Control.Monad.Random
import Control.Monad.Trans.Class

class (Monad m) => Write m where
	getMIORandom :: (R.StdGen -> (a, R.StdGen)) -> m a
	getMIOGen :: m R.StdGen
	setMIOGen :: R.StdGen -> m ()
	newMIOGen :: m R.StdGen

instance Write Change where
	getMIORandom a = Change $! R.getStdRandom a
	getMIOGen = Change $! R.getStdGen
	setMIOGen a = Change $! R.setStdGen a
	newMIOGen = Change $! R.newStdGen

instance Write (Trainer s) where
	getMIORandom a = Trainer $! \_ -> R.getStdRandom a
	getMIOGen = Trainer $! \_ -> R.getStdGen
	setMIOGen a = Trainer $! \_ -> R.setStdGen a
	newMIOGen = Trainer $! \_ -> R.newStdGen