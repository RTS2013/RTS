{-# LANGUAGE DoAndIfThenElse #-}

module Local.Matrices.Sliced.Matrix3D where

import Prelude hiding (read)
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as M
import qualified Control.Monad.ST as ST

make :: Int -> Int -> Int -> a -> V.Vector (V.Vector (V.Vector a))
make x y z a = V.replicate z $ V.replicate y $ V.replicate x a

read :: Int -> Int -> Int -> V.Vector (V.Vector (V.Vector a)) -> Maybe a
read x y z vz = case vz V.!? z of
	Nothing -> Nothing
	Just vy -> case vy V.!? y of
		Nothing -> Nothing
		Just vx -> case vx V.!? x of
			Nothing -> Nothing
			Just ok -> Just ok

write :: Int -> Int -> Int -> a -> V.Vector (V.Vector (V.Vector a)) -> V.Vector (V.Vector (V.Vector a))
write x y z a vz = case vz V.!? z of
	Nothing -> vz
	Just vy -> case vy V.!? y of
		Nothing -> vz
		Just vx -> case vx V.!? x of
			Nothing -> vz
			Just _ -> 
				modix vz $ \vz -> M.write vz z $ 
				modix vy $ \vy -> M.write vy y $
				modix vx $ \vx -> M.write vx x a
  where
	modix = flip V.modify

modify :: Int -> Int -> Int -> (a -> a) -> V.Vector (V.Vector (V.Vector a)) -> V.Vector (V.Vector (V.Vector a))
modify x y z f vz = case vz V.!? z of
	Nothing -> vz
	Just vy -> case vy V.!? y of
		Nothing -> vz
		Just vx -> case vx V.!? x of
			Nothing -> vz
			Just ok -> 
				modix vz $ \vz -> M.write vz z $ 
				modix vy $ \vy -> M.write vy y $
				modix vx $ \vx -> M.write vx x $ f ok
  where
	modix = flip V.modify

bulkWrite :: V.Vector (V.Vector (V.Vector a)) -> [(Int,Int,Int,a)] -> V.Vector (V.Vector (V.Vector a))
bulkWrite vec_z xs = ST.runST $ do
	vz <- V.mapM (V.mapM V.thaw) vec_z
	mapM_ (\(x,y,z,a) -> maybe (return ()) (\v -> M.write v x a) (vz V.!? z >>= (V.!? y))) xs
	V.mapM (V.mapM V.unsafeFreeze) vz

bulkModify :: V.Vector (V.Vector (V.Vector a)) -> [(Int,Int,Int,a -> a)] -> V.Vector (V.Vector (V.Vector a))
bulkModify vec_z xs = ST.runST $ do
	vz <- V.mapM (V.mapM V.thaw) vec_z
	mapM_ (\(x,y,z,f) -> maybe (return ()) (\v -> 
		if x < M.length v 
		then M.read v x >>= \a -> M.write v x (f a) 
		else return ()) (vz V.!? z >>= (V.!? y))) xs
	V.mapM (V.mapM V.unsafeFreeze) vz