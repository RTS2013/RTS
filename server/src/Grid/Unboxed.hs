{-# LANGUAGE BangPatterns, Trustworthy #-}

module Grid.Unboxed
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
import qualified Data.Vector.Unboxed as V
import qualified Grid.UnboxedMutable as M

data Grid a = Grid {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(V.Vector a)

size :: Grid a -> (Int,Int)
size (Grid w h _) = (w,h)

make :: (V.Unbox a) => (Int,Int) -> a -> Grid a
make (w,h) a = Grid w h $ V.replicate (w*h) a

read :: (V.Unbox a) => Grid a -> (Int,Int) -> Maybe a
read (Grid w h vec) (x,y) = 
	if x >= 0 && y >= 0 && x < w && y < h
	then Just (vec V.! (y * w + x))
	else Nothing

readOrElse :: (V.Unbox a) => Grid a -> a -> (Int,Int) -> a
readOrElse (Grid w h vec) a (x,y) = 
	if x >= 0 && y >= 0 && x < w && y < h
	then vec V.! (y * w + x)
	else a

update :: (V.Unbox a) => Grid a -> [(Int,Int,a)] -> Grid a
update (Grid w h vec) = Grid w h . (V.//) vec . map (\(x,y,a) -> (y * w + x,a)) . filter is1Dim
	where
	is1Dim (x,y,_) = x >= 0 && y >= 0 && x < w && y < h

freeze :: (V.Unbox a) => M.Grid a -> IO (Grid a)
freeze (M.Grid w h vec) = fmap (Grid w h) $ V.freeze vec

fromVector :: (V.Unbox a) => (Int,Int) -> V.Vector a -> Grid a
fromVector (w,h) v = Grid w h v

toVector :: (V.Unbox a) => Grid a -> V.Vector a
toVector (Grid _ _ vec) = vec

fromFunction :: (V.Unbox a) => (Int,Int) -> ((Int,Int) -> a) -> Grid a
fromFunction (w,h) f = Grid w h $ V.fromList $ map f [(x,y) | y <- [0..h-1], x <- [0..w-1]]