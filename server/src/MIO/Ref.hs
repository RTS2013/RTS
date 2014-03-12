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

instance Read (Trainer s) where
	read r = Trainer $! \_ -> readIORef r

instance Read (Behavior s) where
	read r = Behavior $! \_ -> readIORef r

instance Read Change where
	read a = Change $! readIORef a

instance Write (Trainer s) where
	make a = Trainer $! \_ -> newIORef a
	write ref a = Trainer $! \_ -> writeIORef ref a
	modify ref f = Trainer $! \_ -> modifyIORef' ref f

instance Write Change where
	make a = Change $! newIORef a
	write ref a = Change $! writeIORef ref a
	modify ref f = Change $! modifyIORef' ref f