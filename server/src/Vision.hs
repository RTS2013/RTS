{-# LANGUAGE Trustworthy #-}

module Vision where

import qualified RIO.Grid as G
import RIO.Prelude

newtype Vision = Vision (G.MGrid Int)

isVisible :: Vision -> (Int,Int) -> RIO priv Bool
isVisible (Vision g) xy = fmap (maybe False (>0)) $ G.read xy g

-- Returns true if the tile was undiscovered
reveal :: Vision -> (Int,Int) -> RIO ReadWrite Bool
reveal (Vision g) xy = do
	isDiscovered <- fmap (maybe True (/=(-1))) $ G.read xy g
	if isDiscovered
	then G.modify xy (+1) g >> return False
	else G.write xy 1 g >> return True

cover :: Vision -> (Int,Int) -> RIO ReadWrite ()
cover (Vision g) xy = do
	isDiscovered <- fmap (maybe True (/=(-1))) $ G.read xy g
	if isDiscovered
	then G.modify xy (\n -> n - 1) g
	else G.write xy (-1) g