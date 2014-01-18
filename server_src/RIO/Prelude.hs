{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Trustworthy #-}

module RIO.Prelude where

import Prelude hiding (read)
import qualified Grid.UnboxedGrid as G
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as V
import Control.Applicative (Applicative(..))
import Control.DeepSeq (NFData)
import System.Random
import RIO.RIO (RIO(..))

instance Functor RIO where
	fmap f (RIO a) = RIO $ fmap f a

instance Applicative RIO where
	pure a = RIO $ return a
	(RIO f) <*> (RIO a) = RIO $ f <*> a

instance Monad RIO where
	return = pure
	(RIO io) >>= f = RIO $ do
		a <- io
		let (RIO b) = f a
		b
	(RIO a) >> (RIO b) = RIO $ a >> b

gridMake :: (M.Unbox a) => (Int,Int) -> a -> RIO (G.MGrid a)
gridMake a b = RIO $ G.make a b

gridRead :: (M.Unbox a) => (Int,Int) -> G.MGrid a -> RIO (Maybe a)
gridRead a b = RIO $ G.read a b

gridWrite :: (M.Unbox a) => (Int,Int) -> a -> G.MGrid a -> RIO ()
gridWrite a b c = RIO $ G.write a b c

gridModify :: (M.Unbox a) => (Int,Int) -> (a -> a) -> G.MGrid a -> RIO ()
gridModify a b c = RIO $ G.modify a b c

gridUnsafeWith :: (V.Unbox a, NFData b) => G.MGrid a -> (G.Grid a -> b) -> RIO b
gridUnsafeWith a b = RIO $ G.unsafeWith a b