module Data where

{-
This is where we will put all of the data for the game. Having all of the data
in one place allows you to really understand the structure of the program much
better than if it's scattered in multiple files. You'll be surprised at how much
info you can fit into such a small space. Also having it all in this file makes
it very easy to reference. import qualified Data as D
-}

import Local.KDT
import Control.DeepSeq

data Unit = Unit { posX, posY, radius, weight :: !Float, reached :: !Bool} deriving (Eq)

instance NFData Unit where

data World = World
	{ units :: ![Unit]
	, units_kdt :: !(KDT Float Unit)
	}
