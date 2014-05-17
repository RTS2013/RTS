{-# LANGUAGE Trustworthy, BangPatterns #-}

module MIO.Grid.Boxed
( Grid()
, Read(..)
, Write(..)
) where

import Prelude hiding (read,Read)
import MIO.MIO
import MIO.Privileges ()
import Grid.BoxedMutable (Grid)
import qualified Grid.BoxedMutable as G
import qualified Grid.Boxed as IG

class (Monad m) => Read m where
	read :: (Int,Int) -> Grid a -> m (Maybe a)

class (Monad m) => Write m where
	make :: (Int,Int) -> a -> m (Grid a)
	write :: (Int,Int) -> a -> Grid a -> m ()
	modify :: (Int,Int) -> (a -> a) -> Grid a -> m ()
	freeze :: Grid a -> m (IG.Grid a)

instance Read (Change s) where
	read a b = Change $! \_ -> G.read a b

instance Read (Behavior s) where
	read a b = Behavior $! \_ -> G.read a b

instance Write (Change s) where
	make a b = Change $! \_ -> G.make a b
	write a b c = Change $! \_ -> G.write a b c
	modify a b c = Change $! \_ -> G.modify a b c
	freeze !a = Change $! \_ -> IG.freeze a