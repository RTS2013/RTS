module Local.Matrices where

import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as M
import qualified Control.Monad.ST as ST
import qualified Control.Concurrent.Spawn as S

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

set2D :: Int -> Int -> a -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
set2D x y a vy = case vy V.!? y of
	Nothing -> vy
	Just vx -> case vx V.!? x of
		Nothing -> vy
		Just _ -> 
			modix vy $ \vy -> M.write vy y $
			modix vx $ \vx -> M.write vx x a
  where
	modix = flip V.modify

set3D :: Int -> Int -> Int -> a -> V.Vector (V.Vector (V.Vector a)) -> V.Vector (V.Vector (V.Vector a))
set3D x y z a vz = case vz V.!? z of
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

bulkUpdate2D :: V.Vector (V.Vector a) -> [(Int,Int,a)] -> V.Vector (V.Vector a)
bulkUpdate2D vec_y xs = ST.runST $ do
	vy <- V.mapM V.thaw vec_y
	mapM_ (\(x,y,a) -> maybe (return ()) (\v -> M.write v x a) (vy V.!? y)) xs
	V.mapM V.unsafeFreeze vy

bulkUpdate3D :: V.Vector (V.Vector (V.Vector a)) -> [(Int,Int,Int,a)] -> V.Vector (V.Vector (V.Vector a))
bulkUpdate3D vec_z xs = ST.runST $ do
	vz <- V.mapM (V.mapM V.thaw) vec_z
	mapM_ (\(x,y,z,a) -> maybe (return ()) (\v -> M.write v x a) (vz V.!? z >>= (V.!? y))) xs
	V.mapM (V.mapM V.unsafeFreeze) vz

