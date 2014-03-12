{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Trustworthy, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module MIO.Privileges
( Change()
, Behavior()
, Trainer()
, behaving
, training
) where

import Control.Applicative (Applicative((<*>),pure))
import MIO.MIO

instance Functor (Trainer s) where
	fmap f (Trainer a) = Trainer $! \s -> fmap f (a s)

instance Applicative (Trainer s) where
	pure a = Trainer $! \_ -> return a
	(Trainer f) <*> (Trainer a) = Trainer $! \s -> (f s) <*> (a s)

instance Monad (Trainer s) where
	return = pure
	(Trainer io) >>= f = Trainer $! \s -> do
		a <- io s
		let (Trainer b) = f a
		b s
	(Trainer a) >> (Trainer b) = Trainer $! \s -> a s >> b s

instance Functor (Behavior s) where
	fmap f (Behavior a) = Behavior $! \s -> fmap f (a s)

instance Applicative (Behavior s) where
	pure a = Behavior $! \_ -> return a
	(Behavior f) <*> (Behavior a) = Behavior $! \s -> (f s) <*> (a s)

instance Monad (Behavior s) where
	return = pure
	(Behavior io) >>= f = Behavior $! \s -> do
		a <- io s
		let (Behavior b) = f a
		b s
	(Behavior a) >> (Behavior b) = Behavior $! \s -> a s >> b s

deriving instance Functor Change
deriving instance Applicative Change
deriving instance Monad Change

behaving :: Behavior s s
behaving = Behavior return

training :: Trainer s s
training = Trainer return