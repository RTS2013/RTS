{-# LANGUAGE DoAndIfThenElse #-}

module Local.Matrices.Matrix2D where

import Prelude hiding (read)
import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V

data IOMatrix a = IOMatrix Int Int (M.IOVector a)
data Matrix a = Matrix Int Int (V.Vector a)

make :: Int -> Int -> a -> IO (IOMatrix a)
make x y a = M.replicate (x*y) a >>= return . IOMatrix x y

read :: Int -> Int -> IOMatrix a -> IO (Maybe a)
read x y (IOMatrix mx my vec) =
	if x >= 0 && y >= 0 && x < mx && y < my
	then M.unsafeRead vec (y * mx + x) >>= return . Just
	else return Nothing

write :: Int -> Int -> a -> IOMatrix a -> IO ()
write x y a (IOMatrix mx _ vec) = M.write vec (y * mx + x) a

modify :: Int -> Int -> (a -> a) -> IOMatrix a -> IO ()
modify x y f m = read x y m >>= maybe (return ()) (\a -> write x y (f a) m)

unsafeWith :: IOMatrix a -> (Matrix a -> b) -> IO b
unsafeWith (IOMatrix x y v) f = V.unsafeFreeze v >>= return . f . Matrix x y