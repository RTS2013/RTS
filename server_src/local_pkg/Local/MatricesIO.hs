module Local.MatricesIO where

import qualified Data.Vector.Unboxed.Mutable as V

data Matrix2D a = Matrix2D Int Int (V.IOVector a)

make2D :: (V.Unbox a) => Int -> Int -> a -> IO (Matrix2D a)
make2D x y a = V.replicate (x*y) a >>= return . Matrix2D x y

get2D :: (V.Unbox a) => Int -> Int -> Matrix2D a -> IO (Maybe a)
get2D x y (Matrix2D mx my vec) =
	if x >= 0 && y >= 0 && x < mx && y < my
	then V.unsafeRead vec (y * mx + x) >>= return . Just
	else return Nothing

set2D :: (V.Unbox a) => Int -> Int -> a -> Matrix2D a -> IO ()
set2D x y a (Matrix2D mx _ vec) = V.write vec (y * mx + x) a

update2D :: (V.Unbox a) => Int -> Int -> (a -> a) -> Matrix2D a -> IO ()
update2D x y f m = get2D x y m >>= maybe (return ()) (\a -> set2D x y (f a) m)