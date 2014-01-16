module Main where

import UnboxedGrid

main = do
	grid <- make (1000,1000) (0 :: Int)
	a <- read (2,2) grid
	sequence $ replicate 1000000 $ do
		modify (2,2) (+1) grid
	b <- read (2,2) grid
	print a
	print b
	return ()