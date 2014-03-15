module Mod.Data where

import qualified Data as D
import MIO.Privileges

data GameS = GameS
	{ 
	}

data TeamS = TeamS
	{ 
	}

data UnitS = UnitS
	{ unitTarget :: !(Maybe (Int,Int))
	, unitPath   :: ![(Int,Int)]
	, moveType   :: !MoveType
	} 

data MoveType = Ground | Flying

type TileS = (Int,Bool)

type Game = D.Game GameS TeamS UnitS TileS

type Team = D.Team GameS TeamS UnitS TileS

type Unit = D.Unit GameS TeamS UnitS TileS

type ModTrainer a = Trainer Game a

type UnitBehavior = Behavior Unit (Change ())

type GameBehavior = Behavior Game (Change ())

type TeamBehavior = Behavior Team (Change ())