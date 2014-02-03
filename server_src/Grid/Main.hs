module Grid.Main where

import qualified Grid.UnboxedGrid as G

main = do
	grid <- G.make (1000,1000) (0 :: Int)
	a <- G.read (2,2) grid
	sequence $ replicate 1000000 $ G.modify (2,2) (+1) grid
	b <- G.read (2,2) grid
	print a
	print b
	return ()