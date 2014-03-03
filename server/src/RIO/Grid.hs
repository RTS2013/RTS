{-# LANGUAGE Trustworthy #-}

module RIO.Grid
( MGrid()
, RIO()
, make
, read
, write
, modify
) where

import Prelude hiding (read)
import RIO.RIO
import RIO.Privileges
import Grid.UnboxedGrid (MGrid)
import qualified Grid.UnboxedGrid as G
import Data.Vector.Unboxed.Mutable (Unbox)

{-# INLINE make #-}
make :: (Unbox a) => (Int,Int) -> a -> RIO ReadWrite (MGrid a)
make a b = RIO $! G.make a b

{-# INLINE read #-}
read :: (Unbox a) => (Int,Int) -> MGrid a -> RIO priv (Maybe a)
read a b = RIO $! G.read a b

{-# INLINE write #-}
write :: (Unbox a) => (Int,Int) -> a -> MGrid a -> RIO ReadWrite ()
write a b c = RIO $! G.write a b c

{-# INLINE modify #-}
modify :: (Unbox a) => (Int,Int) -> (a -> a) -> MGrid a -> RIO ReadWrite ()
modify a b c = RIO $! G.modify a b c