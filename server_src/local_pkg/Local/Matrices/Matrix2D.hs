{-# LANGUAGE DoAndIfThenElse #-}

module Local.Matrices.Matrix2D where

import Prelude hiding (read)
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as V
import Control.Monad.Primitive (PrimState,PrimMonad)

data Matrix v = Matrix Int Int v

make :: (PrimMonad m, M.MVector v a) => Int -> Int -> a -> m (Matrix (v (PrimState m) a))
make x y a = M.replicate (x*y) a >>= return . Matrix x y

read :: (PrimMonad m, M.MVector v a) => Int -> Int -> Matrix (v (PrimState m) a) -> m (Maybe a)
read x y (Matrix mx my vec) =
	if x >= 0 && y >= 0 && x < mx && y < my
	then M.unsafeRead vec (y * mx + x) >>= return . Just
	else return Nothing

write :: (PrimMonad m, M.MVector v a) => Int -> Int -> a -> Matrix (v (PrimState m) a) -> m ()
write x y a (Matrix mx _ vec) = M.write vec (y * mx + x) a

modify :: (PrimMonad m, M.MVector v a) => Int -> Int -> (a -> a) -> Matrix (v (PrimState m) a) -> m ()
modify x y f m = read x y m >>= maybe (return ()) (\a -> write x y (f a) m)

unsafeWith :: (V.Vector v a, PrimMonad m) => Matrix (V.Mutable v (PrimState m) a) -> (Matrix (v a) -> b) -> m b
unsafeWith (Matrix x y v) f = V.unsafeFreeze v >>= return . f . Matrix x y