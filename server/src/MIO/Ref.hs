{-# LANGUAGE Trustworthy #-}

module MIO.Ref
( Read(..)
, Write(..)
) where

import Prelude hiding (read,Read)
import MIO.MIO
import MIO.Privileges ()
import Data.IORef

type Ref = IORef

class (Monad m) => Read m where
	read :: Ref a -> m a

class (Monad m) => Write m where
	make :: a -> m (Ref a)
	write :: Ref a -> a -> m ()
	modify :: Ref a -> (a -> a) -> m ()

instance Read (Change s) where
	read r = Change $! \_ -> readIORef r

instance Read (Behavior s) where
	read r = Behavior $! \_ -> readIORef r

instance Write (Change s) where
	make a = Change $! \_ -> a `seq` newIORef a
	write ref a = Change $! \_ -> a `seq` writeIORef ref a
	modify ref f = Change $! \_ -> modifyIORef' ref f