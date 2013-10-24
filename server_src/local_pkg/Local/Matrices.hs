module Local.Matrices where

import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as M

make2D :: Int -> Int -> a -> V.Vector (V.Vector a)
make2D x y a = V.replicate y $ V.replicate x a

make3D :: Int -> Int -> Int -> a -> V.Vector (V.Vector (V.Vector a))
make3D x y z a = V.replicate z $ V.replicate y $ V.replicate x a

get2D :: Int -> Int -> V.Vector (V.Vector a) -> Maybe a
get2D x y vy = case vy V.!? y of
	Nothing -> Nothing
	Just vx -> case vx V.!? x of
		Nothing -> Nothing
		Just ok -> Just ok

get3D :: Int -> Int -> Int -> V.Vector (V.Vector (V.Vector a)) -> Maybe a
get3D x y z vz = case vz V.!? z of
	Nothing -> Nothing
	Just vy -> case vy V.!? y of
		Nothing -> Nothing
		Just vx -> case vx V.!? x of
			Nothing -> Nothing
			Just ok -> Just ok

update2D :: Int -> Int -> (a -> a) -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
update2D x y f vy = case vy V.!? y of
	Nothing -> vy
	Just vx -> case vx V.!? x of
		Nothing -> vy
		Just ok -> 
			modix vy $ \vy -> M.write vy y $
			modix vx $ \vx -> M.write vx x $ f ok
  where
	modix = flip V.modify

update3D :: Int -> Int -> Int -> (a -> a) -> V.Vector (V.Vector (V.Vector a)) -> V.Vector (V.Vector (V.Vector a))
update3D x y z f vz = case vz V.!? z of
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