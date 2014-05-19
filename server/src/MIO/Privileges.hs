{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Trustworthy #-}

module MIO.Privileges
( Change()
, Behavior()
, behaving
, changing
, putStrLnMIO
) where

import Control.Applicative (Applicative((<*>),pure))
import MIO.MIO

instance Functor (Change s) where
	fmap f (Change a) = Change $! \s -> fmap f (a s)

instance Applicative (Change s) where
	pure a = Change $! \_ -> return a
	(Change f) <*> (Change a) = Change $! \s -> (f s) <*> (a s)

instance Monad (Change s) where
	return = pure
	(Change io) >>= f = Change $! \s -> do
		a <- io s
		let (Change b) = f a
		b s
	(Change a) >> (Change b) = Change $! \s -> a s >> b s

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

behaving :: Behavior s s
behaving = Behavior return

changing :: Change s s
changing = Change return

putStrLnMIO :: String -> Change s ()
putStrLnMIO str = Change $! \_ -> putStrLn str