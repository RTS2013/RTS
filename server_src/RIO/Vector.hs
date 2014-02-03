{-# LANGUAGE Trustworthy #-}

module RIO.Vector
( make
, read
, write
, modify
) where

import Prelude hiding (read)
import RIO.RIO
import RIO.Prelude
import qualified Data.Vector.Mutable as M

{-# INLINE make #-}
make :: Int -> a -> RIO ReadWrite (M.IOVector a)
make n a = RIO $! M.replicate n a

{-# INLINE read #-}
read :: M.IOVector a -> Int -> RIO priv (Maybe a)
read v i = RIO $! if i < M.length v && i >= 0 then M.read v i >>= \a -> (return $! Just $! a) else return Nothing

{-# INLINE write #-}
write :: M.IOVector a -> Int -> a -> RIO ReadWrite ()
write v i a = RIO $! M.write v i a

{-# INLINE modify #-}
modify :: M.IOVector a -> Int -> (a -> a) -> RIO ReadWrite ()
modify v i f = read v i >>= maybe (return ()) (write v i . f)