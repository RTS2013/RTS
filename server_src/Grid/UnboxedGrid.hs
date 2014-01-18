{-# LANGUAGE BangPatterns, Trustworthy #-}

module Grid.UnboxedGrid
( MGrid
, Grid
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
import Control.Monad.Primitive (PrimState)
import Control.DeepSeq (NFData)

data Grid a = Grid {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(V.Vector a)
data MGrid a = MGrid {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(V.MVector (PrimState IO) a)

make :: (M.Unbox a) => (Int,Int) -> a -> IO (MGrid a)
make (!x,!y) !a = let xp = M.replicate (x*y) a >>= return . MGrid x y in
	xp `seq` xp

read :: (M.Unbox a) => (Int,Int) -> MGrid a -> IO (Maybe a)
read (!x,!y) (MGrid !mx !my !vec) = 
	let xp = if x >= 0 && y >= 0 && x < mx && y < my
		      then M.unsafeRead vec (y * mx + x) >>= return . Just
		      else return Nothing in
	xp `seq` xp

write :: (M.Unbox a) => (Int,Int) -> a -> MGrid a -> IO ()
write (!x,!y) !a (MGrid !mx !_ !vec) = let xp = M.write vec (y * mx + x) a in
	xp `seq` xp

modify :: (M.Unbox a) => (Int,Int) -> (a -> a) -> MGrid a -> IO ()
modify !c !f !m = let xp = read c m >>= maybe (return ()) (\a -> write c (f a) m) in
	xp `seq` xp

unsafeWith :: (V.Unbox a, NFData b) => MGrid a -> (Grid a -> b) -> IO b
unsafeWith (MGrid !x !y !v) !f = let xp = V.unsafeFreeze v >>= return . f . Grid x y in
	xp `seq` xp

(!?) :: (M.Unbox a) => Grid a -> (Int,Int) -> Maybe a
(!?) (Grid !w !h !v) (!x,!y) = 
    if x >= 0 && y >= 0 && x < w && y < h
    then Just $ v V.! (y * w + x)
    else Nothing 