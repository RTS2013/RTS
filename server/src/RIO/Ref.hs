{-# LANGUAGE Trustworthy #-}

module RIO.Ref
( make
, read
, write
, modify
) where

import Prelude hiding (read)
import RIO.RIO
import RIO.Privileges
import Data.IORef

type Ref = IORef

make :: a -> RIO ReadWrite (Ref a)
make = RIO . newIORef

read :: Ref a -> RIO priv a
read = RIO . readIORef

write :: Ref a -> a -> RIO ReadWrite ()
write ref = RIO . writeIORef ref

modify :: Ref a -> (a -> a) -> RIO ReadWrite ()
modify ref = RIO . modifyIORef' ref