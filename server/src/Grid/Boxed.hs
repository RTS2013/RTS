{-# LANGUAGE BangPatterns, Trustworthy #-}

module Grid.Boxed
( Grid
, size
, make
, read
, readOrElse
, update
, freeze
, fromVector
, toVector
, fromFunction
) where

import Prelude hiding (read)
import qualified Data.Vector as V
import qualified Grid.BoxedMutable as M

data Grid a = Grid {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(V.Vector a)

size :: Grid a -> (Int,Int)
size (Grid w h _) = (w,h)

make :: (Int,Int) -> a -> Grid a
make (w,h) a = Grid w h $ V.replicate (w*h) a

read :: Grid a -> (Int,Int) -> Maybe a
read (Grid w h vec) (x,y) = 
	if x >= 0 && y >= 0 && x < w && y < h
	then vec V.!? (y * w + x)
	else Nothing

readOrElse :: Grid a -> a -> (Int,Int) -> a
readOrElse (Grid w h vec) a (x,y) = 
	if x >= 0 && y >= 0 && x < w && y < h
	then vec V.! (y * w + x)
	else a

update :: Grid a -> [(Int,Int,a)] -> Grid a
update (Grid w h vec) = Grid w h . (V.//) vec . map (\(x,y,a) -> (y * w + x,a)) . filter is1Dim
	where
	is1Dim (x,y,_) = x >= 0 && y >= 0 && x < w && y < h

freeze :: M.Grid a -> IO (Grid a)
freeze (M.Grid w h vec) = fmap (Grid w h) $ V.freeze vec

fromVector :: (Int,Int) -> V.Vector a -> Grid a
fromVector (w,h) v = Grid w h v

toVector :: Grid a -> V.Vector a
toVector (Grid _ _ vec) = vec

fromFunction :: (Int,Int) -> ((Int,Int) -> a) -> Grid a
fromFunction (w,h) f = Grid w h $ V.fromList $ map f [(x,y) | y <- [0..h-1], x <- [0..w-1]]