{-# LANGUAGE Trustworthy #-}

module MIO.HashTable 
( Read(..)
, Write(..)
) where

import Prelude hiding (read,Read)
import MIO.MIO
import MIO.Privileges ()
import Data.Hashable (Hashable)
import qualified Data.HashTable.IO as H

type HashTable k v = H.BasicHashTable k v

class (Monad m) => Read m where
	read :: (Eq k, Hashable k) => HashTable k v -> k -> m (Maybe v)

class (Monad m) => Write m where
	make :: Int -> m (HashTable k v)
	write :: (Eq k, Hashable k) => HashTable k v -> k -> v -> m ()
	delete :: (Eq k, Hashable k) => HashTable k v -> k -> m ()
	modify :: (Eq k, Hashable k) => HashTable k v -> k -> (v -> v) -> m ()

instance Read (Change s) where
	read a b = Change $! \_ -> H.lookup a b

instance Read (Behavior s) where
	read a b = Behavior $! \_ -> H.lookup a b

instance Write (Change s) where
	make a = Change $! \_ -> H.newSized a
	write a b c = Change $! \_ -> H.insert a b c
	delete a b = Change $! \_ -> H.delete a b
	modify a b c = read a b >>= maybe (return ()) (write a b . c)