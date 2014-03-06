module Mod 
( runMod
, defaultGameState
, defaultTileState
, defaultTeamState
) where

import Data
import RIO.Privileges
import RIO.Prelude
import RIO.Random
import qualified Data.IntMap as IM

{-
import qualified RIO.HashTable as HT
import qualified RIO.Ref as Ref
import qualified RIO.Grid as Grid
import qualified Data.IntMap as IM

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

type TileS = (Int,Bool)

type TheGame = Game GameS TeamS UnitS TileS

type TheUnit = Unit GameS TeamS UnitS TileS

type TheTeam = Team GameS TeamS UnitS TileS

defaultGameState :: GameS
defaultGameState = GameS

defaultTileState :: TileS
defaultTileState = (-1,True)

defaultTeamState :: TeamS
defaultTeamState = TeamS

runMod :: Int -> TheGame -> RIO ReadWrite TheGame
runMod nTeams game = do
	rng <- getRIOGen
	flip evalRandT rng $ do
		flip mapM_ [0..nTeams-1] $ \nTeam -> do
			flip mapM_ [0..20::Int] $ \_ -> do
				x <- getRandomR (0,1024)
				y <- getRandomR (0,1024)
				a <- getRandomR (0,2*pi)
				lift $ makeUnit game (defaultUnit UnitS) nTeam (x,y,0,a)
	return game

genericGroundUnit :: TheGame -> TheUnit
genericGroundUnit game = (defaultUnit UnitS) 
	{ unitBehaviors = IM.singleton 0 $ \u -> do
		undefined
	}