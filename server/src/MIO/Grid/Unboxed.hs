{-# LANGUAGE Trustworthy, BangPatterns #-}

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
import qualified Grid.Unboxed as IG

class (Monad m) => Read m where
	read :: (Unbox a) => (Int,Int) -> Grid a -> m (Maybe a)

class (Monad m) => Write m where
	make :: (Unbox a) => (Int,Int) -> a -> m (Grid a)
	write :: (Unbox a) => (Int,Int) -> a -> Grid a -> m ()
	modify :: (Unbox a) => (Int,Int) -> (a -> a) -> Grid a -> m ()
	freeze :: (Unbox a) => Grid a -> m (IG.Grid a)

instance Read (Change s) where
	read a b = Change $! \_ -> G.read a b

instance Read (Behavior s) where
	read a b = Behavior $! \_ -> G.read a b

instance Write (Change s) where
	make a b = Change $! \_ -> G.make a b
	write a b c = Change $! \_ -> G.write a b c
	modify a b c = Change $! \_ -> G.modify a b c
	freeze !a = Change $! \_ -> IG.freeze a