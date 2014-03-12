{-# LANGUAGE BangPatterns, Trustworthy #-}

module Grid.Unboxed
( Grid
, size
, make
, read
, freeze
) where

import Prelude hiding (read)
import qualified Data.Vector.Unboxed as V
import qualified Grid.UnboxedMutable as M

data Grid a = Grid {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(V.Vector a)

size :: Grid a -> (Int,Int)
size (Grid mx my _) = (mx,my)

make :: (V.Unbox a) => (Int,Int) -> a -> Grid a
make (x,y) a = Grid x y $ V.replicate (x*y) a

read :: (V.Unbox a) => Grid a -> (Int,Int) -> Maybe a
read (Grid mx my vec) (x,y) = 
	if x >= 0 && y >= 0 && x < mx && y < my
	then vec V.!? (y * mx + x)
	else Nothing

freeze :: (V.Unbox a) => M.Grid a -> IO (Grid a)
freeze (M.Grid x y vec) = fmap (Grid x y$) $ V.freeze vec