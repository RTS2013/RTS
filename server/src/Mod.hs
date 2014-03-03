module Mod (runMod) where

import Data
import RIO.Privileges
import RIO.Prelude
import RIO.Ref

{-

DD = damage dealt
DT = damage taken

(DD + DT)*0.5 + sqrt( (DD*a) * (DT*b)
where a + b = 1
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

runMod :: Int -> Game gameS teamS unitS tileS -> RIO ReadWrite (Game GameS TeamS UnitS TileS)
runMod nTeams defGame = undefined {-do
	return $ defGame
		{ 
		}
		-}