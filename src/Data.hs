module Data where

import KDT
import Control.DeepSeq

data Unit = Unit { posX, posY, radius, weight :: !Float, reached :: !Bool} deriving (Eq)

instance NFData Unit where

data World = World
	{ units :: ![Unit]
	, units_kdt :: !(KDT Float Unit)
	}