{-# LANGUAGE Trustworthy #-}

module MIO.Pathing where

import MIO.MIO
import MIO.Privileges ()
import Pathing (setGroupsM)
import Data.Vector.Unboxed (Vector)

class (Monad m) => Pathaway m where
	setGroupsMIO ::
	    (Int,Int) -> -- Width/Height
	    ((Int,Int) -> IO Bool) -> -- Is open predicate
	    ((Int,Int) -> Vector (Int,Int) -> IO ()) -> -- Set group at point
	    m ()

instance Pathaway (Trainer s) where
	setGroupsMIO wh isOpenM setCornersM = 
		Trainer $! \_ -> setGroupsM wh isOpenM setCornersM