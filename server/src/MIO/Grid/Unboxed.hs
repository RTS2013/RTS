{-# LANGUAGE Trustworthy #-}

module MIO.Grid.Unboxed
( Grid()
, Read(..)
, Write(..)
) where

import Prelude hiding (read,Read)
import MIO.MIO
import MIO.Privileges ()
import Data.Vector.Unboxed (Unbox)
import Grid.UnboxedMutable (Grid)
import qualified Grid.UnboxedMutable as G

class (Monad m) => Read m where
	read :: (Unbox a) => (Int,Int) -> Grid a -> m (Maybe a)

class (Monad m) => Write m where
	make :: (Unbox a) => (Int,Int) -> a -> m (Grid a)
	write :: (Unbox a) => (Int,Int) -> a -> Grid a -> m ()
	modify :: (Unbox a) => (Int,Int) -> (a -> a) -> Grid a -> m ()

instance Read (Trainer s) where
	read a b = Trainer $! \_ -> G.read a b

instance Read (Behavior s) where
	read a b = Behavior $! \_ -> G.read a b

instance Read Change where
	read a b = Change $! G.read a b

instance Write (Trainer s) where
	make a b = Trainer $! \_ -> G.make a b
	write a b c = Trainer $! \_ -> G.write a b c
	modify a b c = Trainer $! \_ -> G.modify a b c

instance Write Change where
	make a b = Change $! G.make a b
	write a b c = Change $! G.write a b c
	modify a b c = Change $! G.modify a b c