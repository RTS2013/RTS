{-# LANGUAGE BangPatterns, Trustworthy #-}

module Grid.Boxed
( Grid
, size
, make
, read
, freeze
) where

import Prelude hiding (read)
import qualified Data.Vector as V
import qualified Grid.BoxedMutable as M

data Grid a = Grid {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(V.Vector a)

size :: Grid a -> (Int,Int)
size (Grid mx my _) = (mx,my)

make :: (Int,Int) -> a -> Grid a
make (x,y) a = Grid x y $ V.replicate (x*y) a

read :: Grid a -> (Int,Int) -> Maybe a
read (Grid mx my vec) (x,y) = 
	if x >= 0 && y >= 0 && x < mx && y < my
	then vec V.!? (y * mx + x)
	else Nothing

freeze :: M.Grid a -> IO (Grid a)
freeze (M.Grid x y vec) = fmap (Grid x y$) $ V.freeze vec