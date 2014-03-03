{-# LANGUAGE Trustworthy #-}

module RIO.HashTable 
( make
, read
, write
, delete
, modify
) where

import Prelude hiding (read)
import RIO.RIO
import RIO.Privileges
import Data.Hashable (Hashable)
import qualified Data.HashTable.IO as H

type HashTable k v = H.BasicHashTable k v

{-# INLINE make #-}
make :: Int -> RIO ReadWrite (HashTable k v)
make a = RIO $ H.newSized a

{-# INLINE read #-}
read :: (Eq k, Hashable k) => HashTable k v -> k -> RIO priv (Maybe v)
read a b = RIO $ H.lookup a b

{-# INLINE write #-}
write :: (Eq k, Hashable k) => HashTable k v -> k -> v -> RIO ReadWrite ()
write a b c = RIO $ H.insert a b c

{-# INLINE delete #-}
delete :: (Eq k, Hashable k) => HashTable k v -> k -> RIO ReadWrite ()
delete a b = RIO $ H.delete a b

{-# INLINE modify #-}
modify :: (Eq k, Hashable k) => HashTable k v -> k -> (v -> v) -> RIO ReadWrite ()
modify a b c = read a b >>= maybe (return ()) (write a b . c)