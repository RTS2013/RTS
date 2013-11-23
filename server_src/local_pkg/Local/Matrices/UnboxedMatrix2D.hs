{-# LANGUAGE DoAndIfThenElse #-}

module Local.Matrices.UnboxedMatrix2D
( Matrix(..)
, make
, read
, write
, modify
, unsafeWith
, (!?)
) where

import Prelude hiding (read)
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as V
import Control.Monad.Primitive (PrimState,PrimMonad)
import Control.DeepSeq (deepseq,NFData)

data Matrix v = Matrix {-# UNPACK #-} !Int {-# UNPACK #-} !Int !v

make :: (M.Unbox a, PrimMonad m) => Int -> Int -> a -> m (Matrix (V.MVector (PrimState m) a))
make x y a = M.replicate (x*y) a >>= return . Matrix x y

read :: (M.Unbox a, PrimMonad m) => Int -> Int -> Matrix (V.MVector (PrimState m) a) -> m (Maybe a)
read x y (Matrix mx my vec) =
	if x >= 0 && y >= 0 && x < mx && y < my
	then M.unsafeRead vec (y * mx + x) >>= return . Just
	else return Nothing

write :: (M.Unbox a, PrimMonad m) => Int -> Int -> a -> Matrix (V.MVector (PrimState m) a) -> m ()
write x y a (Matrix mx _ vec) = M.write vec (y * mx + x) a

modify :: (M.Unbox a, PrimMonad m) => Int -> Int -> (a -> a) -> Matrix (V.MVector (PrimState m) a) -> m ()
modify x y f m = read x y m >>= maybe (return ()) (\a -> write x y (f a) m)

unsafeWith :: (PrimMonad m, V.Unbox a, NFData b) => Matrix (V.MVector (PrimState m) a) -> (Matrix (V.Vector a) -> b) -> m b
unsafeWith (Matrix x y v) f = V.unsafeFreeze v >>= return . f . Matrix x y >>= (\b -> b `deepseq` return b)

(!?) :: (M.Unbox a) => Matrix (V.Vector a) -> (Int,Int) -> Maybe a
(!?) (Matrix w h v) (x,y) = v V.!? (w * y + x)

main = do
    m <- make 1028 1028 (0 :: Int)
    write 5 0 1 m
    b <- unsafeWith m (\(Matrix _ _ v) -> v V.! 5)
    write 5 0 2 m
    c <- unsafeWith m (\(Matrix _ _ v) -> v V.! 5)
    print b
    print c