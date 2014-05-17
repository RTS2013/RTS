{-# LANGUAGE Trustworthy, BangPatterns #-}

module MIO.Vector
( Read(..)
, Write(..)
) where

import Prelude hiding (read,Read)
import MIO.MIO
import MIO.Privileges ()
import qualified Data.Vector.Mutable as M

class (Monad m) => Read m where
    read :: M.IOVector a -> Int -> m (Maybe a)

class (Monad m) => Write m where
    make :: Int -> a -> m (M.IOVector a)
    write :: M.IOVector a -> Int -> a -> m ()
    modify :: M.IOVector a -> Int -> (a -> a) -> m ()

instance Read (Change s) where
    read !v i = Change $! \_ -> if i < M.length v && i >= 0 then M.read v i >>= 
               \a -> (return $! Just $! a) else return Nothing

instance Read (Behavior s) where
    read !v i = Behavior $! \_ -> if i < M.length v && i >= 0 then M.read v i >>= 
               \a -> (return $! Just $! a) else return Nothing

instance Write (Change s) where
    make n a = Change $! \_ -> M.replicate n a
    write !v i a = Change $! \_ -> M.write v i a
    modify !v i f = read v i >>= maybe (return ()) (write v i . f)