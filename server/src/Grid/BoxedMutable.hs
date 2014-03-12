{-# LANGUAGE BangPatterns, Trustworthy #-}

module Grid.BoxedMutable
( Grid(..)
, size
, make
, read
, write
, modify
) where

import Prelude hiding (read)
import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V
import Control.Monad.Primitive (PrimState)

data Grid a = Grid {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(V.MVector (PrimState IO) a)

size :: Grid a -> (Int,Int)
size (Grid mx my _) = (mx,my)

make :: (Int,Int) -> a -> IO (Grid a)
make (x,y) a = M.replicate (x*y) a >>= return . Grid x y

read :: (Int,Int) -> Grid a -> IO (Maybe a)
read (x,y) (Grid mx my vec) = 
	if x >= 0 && y >= 0 && x < mx && y < my
	then M.unsafeRead vec (y * mx + x) >>= return . Just
	else return Nothing

write :: (Int,Int) -> a -> Grid a -> IO ()
write (x,y) a (Grid mx _ vec) = M.write vec (y * mx + x) a

modify :: (Int,Int) -> (a -> a) -> Grid a -> IO ()
modify c f m = read c m >>= maybe (return ()) (\a -> write c (f a) m)