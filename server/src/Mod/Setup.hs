{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Mod.Setup
( runMod
, defaultGameState
, defaultNodeState
, defaultTeamState
) where

import qualified Data.IntMap      as IM
import qualified Data.Vector.Unboxed as UV
import qualified MIO.HashTable    as HT
import qualified MIO.Ref          as Ref
import qualified MIO.Grid.Unboxed as GUM -- Grid Unboxed Mutable
import qualified MIO.Grid.Boxed   as GBM -- Grid Boxed Mutable
import qualified Grid.Unboxed     as GU  -- Grid Unboxed Immutable
import qualified Grid.Boxed       as GB  -- Grid Boxed Immutable
import qualified Movers           as Move
import qualified KDTree           as KDT
import MIO.Privileges
import Mod.Prelude
import MIO.Random
import Data

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

defaultGameState :: GameS
defaultGameState = GameS

defaultNodeState :: NodeS
defaultNodeState = (0,True)

defaultTeamState :: TeamS
defaultTeamState = TeamS

defaultUnitState :: UnitS
defaultUnitState = UnitS 
	{ moveType = Ground
	}

runMod :: Int -> ModChange ()
runMod nTeams = do
	rng <- getMIOGen
	flip evalRandT rng $ do
		flip mapM_ [0..nTeams-1] $ \nTeam -> do
			flip mapM_ [0..199::Int] $ \_ -> do
				x <- getRandomR (497,527)
				y <- getRandomR (497,527)
				a <- getRandomR (0,2*pi)
				lift $ makeUnit (defaultUnit defaultUnitState) nTeam (x,y,0,a)
	