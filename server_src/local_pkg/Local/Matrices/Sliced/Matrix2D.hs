{-# LANGUAGE DoAndIfThenElse #-}

module Local.Matrices.Sliced.Matrix2D where

import Prelude hiding (read)
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as M
import qualified Control.Monad.ST as ST

make :: Int -> Int -> a -> V.Vector (V.Vector a)
make x y a = V.replicate y $ V.replicate x a

read :: Int -> Int -> V.Vector (V.Vector a) -> Maybe a
read x y vy = case vy V.!? y of
	Nothing -> Nothing
	Just vx -> case vx V.!? x of
		Nothing -> Nothing
		Just ok -> Just ok

write :: Int -> Int -> a -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
write x y a vy = case vy V.!? y of
	Nothing -> vy
	Just vx -> case vx V.!? x of
		Nothing -> vy
		Just _ -> 
			modix vy $ \vy -> M.write vy y $
			modix vx $ \vx -> M.write vx x a
  where
	modix = flip V.modify

modify :: Int -> Int -> (a -> a) -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
modify x y f vy = case vy V.!? y of
	Nothing -> vy
	Just vx -> case vx V.!? x of
		Nothing -> vy
		Just ok -> 
			modix vy $ \vy -> M.write vy y $
			modix vx $ \vx -> M.write vx x $ f ok
  where
	modix = flip V.modify

bulkWrite :: V.Vector (V.Vector a) -> [(Int,Int,a)] -> V.Vector (V.Vector a)
bulkWrite vec_y xs = ST.runST $ do
	vy <- V.mapM V.thaw vec_y
	mapM_ (\(x,y,a) -> maybe (return ()) (\v -> M.write v x a) (vy V.!? y)) xs
	V.mapM V.unsafeFreeze vy

bulkModify :: V.Vector (V.Vector a) -> [(Int,Int,a -> a)] -> V.Vector (V.Vector a)
bulkModify vec_y xs = ST.runST $ do
	vy <- V.mapM V.thaw vec_y
	mapM_ (\(x,y,f) -> maybe (return ()) (\v -> 
		if x < M.length v 
        then M.read v x >>= \a -> M.write v x (f a) 
        else return ()) (vy V.!? y)) xs
	V.mapM V.unsafeFreeze vy