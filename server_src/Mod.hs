module Mod (runMod) where

import Data

{-
DD = damage dealt
DT = damage taken

(DD + DT)/2 + sqrt( (DD/2) * (DT/2)
-}

data GameS = GameS
	{
	}
data TeamS = TeamS
	{ 
	}
data UnitS = UnitS
	{
	}
data TileS = TileS
	{
	}

runMod :: RIO ReadWrite (Game GameS TeamS UnitS TileS)
runMod = undefined