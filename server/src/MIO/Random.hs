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

instance Write (Change s) where
	getMIORandom a = Change $! \_ -> R.getStdRandom a
	getMIOGen = Change $! \_ -> R.getStdGen
	setMIOGen a = Change $! \_ -> R.setStdGen a
	newMIOGen = Change $! \_ -> R.newStdGen