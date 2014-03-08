{-# LANGUAGE BangPatterns, Trustworthy #-}

module Grid.UnboxedGrid
( MGrid
, make
, read
, write
, modify
) where

import Prelude hiding (read)
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as V
import Control.Monad.Primitive (PrimState)

data MGrid a = MGrid {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(V.MVector (PrimState IO) a)

size :: MGrid a -> (Int,Int)
size (MGrid mx my _) = (mx,my)

{-# INLINE make #-}
make :: (M.Unbox a) => (Int,Int) -> a -> IO (MGrid a)
make (x,y) a = M.replicate (x*y) a >>= return . MGrid x y

{-# INLINE read #-}
read :: (M.Unbox a) => (Int,Int) -> MGrid a -> IO (Maybe a)
read (x,y) (MGrid mx my vec) = 
	if x >= 0 && y >= 0 && x < mx && y < my
	then M.unsafeRead vec (y * mx + x) >>= return . Just
	else return Nothing

{-# INLINE write #-}
write :: (M.Unbox a) => (Int,Int) -> a -> MGrid a -> IO ()
write (x,y) a (MGrid mx _ vec) = M.write vec (y * mx + x) a

{-# INLINE modify #-}
modify :: (M.Unbox a) => (Int,Int) -> (a -> a) -> MGrid a -> IO ()
modify c f m = read c m >>= maybe (return ()) (\a -> write c (f a) m)