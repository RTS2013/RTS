module Mod.Data where

import Data
import MIO.Privileges

data GameS = GameS
    { 
    }

data UnitS = UnitS
    { moveType :: !MoveType
    } 

data TeamS = TeamS
    { 
    }

type NodeS = (Int,Bool)

data MoveType = Ground

{-
type ModGame = Game GameS UnitS TeamS

type ModTeam = Team GameS UnitS TeamS

type ModUnit = Unit GameS UnitS TeamS

type UnitBehavior = PBehavior ModGame ModUnit ModTeam
-}
type ModChange a = Change (Game GameS UnitS TeamS) a