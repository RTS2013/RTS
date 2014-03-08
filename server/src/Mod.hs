module Mod 
( runMod
, defaultGameState
, defaultTileState
, defaultTeamState
) where

import qualified Data.IntMap as IM
--import qualified RIO.HashTable as HT
import qualified RIO.Ref as Ref
--import qualified RIO.Grid as Grid
import qualified Movement as M
import qualified KDT as KDT
import Data.Sequence (viewl,ViewL(..))
import RIO.Privileges
import RIO.Prelude
import RIO.Random
import Data

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

type TileS = (Int,Bool)

type ModGame = Game GameS TeamS UnitS TileS

type ModTeam = Team GameS TeamS UnitS TileS

type ModUnit = Unit GameS TeamS UnitS TileS

type ModBehavior a = Behavior GameS TeamS UnitS TileS a

defaultGameState :: GameS
defaultGameState = GameS

defaultTileState :: TileS
defaultTileState = (-1,True)

defaultTeamState :: TeamS
defaultTeamState = TeamS

runMod :: Int -> ModGame -> RIO ReadWrite ModGame
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

genericGroundUnit :: ModGame -> ModUnit
genericGroundUnit game = (defaultUnit UnitS) 
	{ unitBehaviors = IM.singleton 0 $ \u -> do
		undefined
	}

firstUnit :: ModGame -> ModUnit
firstUnit game = (genericGroundUnit game)
	{ unitBehaviors = IM.singleton 0 $ \u -> return $ do
		case viewl (unitOrders u) of
			EmptyL -> return ()
			(a:<_) -> undefined
	}

maxRadius :: (Num a) => a
maxRadius = 5

moveDistToPoint :: ModGame -> Float -> Float -> Float -> ModBehavior Unit
moveDistToPoint g dx dy dist u = do
	kdt <- Ref.read (gameKDT g)
	return $ do
		modifyUnit g (unitTeam u) (unitID u) $ M.move2D 
			(\_ _ -> True) 
			(\x y r -> KDT.inRange 
				kdt 
				[ positionX . unitMoveState, positionY . unitMoveState ] 
				[x,y] 
				r)
			maxRadius
			dx
			dy
			dist