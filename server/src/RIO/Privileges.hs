{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Trustworthy #-}

module RIO.Privileges
( ReadOnly
, ReadWrite
, RIO()
) where

import Control.Applicative (Applicative(..))
import RIO.RIO

data ReadOnly
data ReadWrite

instance Functor (RIO mode) where
	fmap f (RIO a) = RIO $ fmap f a

instance Applicative (RIO mode) where
	pure a = RIO $ return $ a
	(RIO f) <*> (RIO a) = RIO $ f <*> a

instance Monad (RIO mode) where
	return = pure
	(RIO io) >>= f = RIO $ do
		a <- io
		let (RIO b) = f a
		b
	(RIO a) >> (RIO b) = RIO $ a >> b