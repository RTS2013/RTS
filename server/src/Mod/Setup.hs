{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Mod.Setup
( runMod
, defaultGameState
, defaultTileState
, defaultTeamState
) where

import qualified Data.IntMap      as IM
import qualified MIO.HashTable    as HT
import qualified MIO.Ref          as Ref
import qualified MIO.Grid.Unboxed as GUM -- Grid Unboxed Mutable
import qualified MIO.Grid.Boxed   as GBM -- Grid Boxed Mutable
import qualified Grid.Unboxed     as GU  -- Grid Unboxed Immutable
import qualified Grid.Boxed       as GB  -- Grid Boxed Immutable
import qualified Movers           as Move
import qualified KDTree           as KDT
import Data.Sequence (viewl,ViewL(..))
import MIO.Privileges
import Mod.Prelude
import MIO.Random
import Pathing
import Data

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

data MoveType = Ground

type TileS = (Int,Bool)

type ModGame = Game GameS TeamS UnitS TileS

-- type ModTeam = Team GameS TeamS UnitS TileS

type ModUnit = Unit GameS TeamS UnitS TileS

type UnitBehavior a = Behavior ModUnit a

defaultGameState :: GameS
defaultGameState = GameS

defaultTileState :: TileS
defaultTileState = (-1,True)

defaultTeamState :: TeamS
defaultTeamState = TeamS

defaultUnitState :: UnitS
defaultUnitState = UnitS 
	{ unitTarget = Nothing
	, unitPath   = []
	, moveType   = Ground
	}

runMod :: Int -> Trainer ModGame ()
runMod nTeams = do
	rng <- getMIOGen
	flip evalRandT rng $ do
		flip mapM_ [0..nTeams-1] $ \nTeam -> do
			flip mapM_ [0..99::Int] $ \_ -> do
				x <- getRandomR (0,1024)
				y <- getRandomR (0,1024)
				a <- getRandomR (0,2*pi)
				lift $ makeUnit (defaultUnit defaultUnitState) nTeam (x,y,0,a)

{-
handleOrders :: Trainer ModGame (UnitBehavior (Change ()))
handleOrders = do
	game <- training
	return $ do
		u <- behaving
		case viewl (unitOrders u) of
				EmptyL -> return $ return ()
				(a:<_) -> case a of
					(Move p) -> handleMoveOrder game p
					_ -> return $ return ()

handleMoveOrder :: ModGame -> Point -> UnitBehavior (Change ())
handleMoveOrder (Point x y _) = do
	u <- behaving
	let (ux,uy) = getUnitPosition u
	if null $ unitPath $ unitState u
	then getPath 
	else undefined
-}