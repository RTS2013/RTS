{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Trustworthy #-}

module RIO.Prelude where

import Data
import RIO.Privileges
import qualified RIO.Ref as Ref
import qualified RIO.Grid as Grid
import qualified RIO.HashTable as HT
import qualified Data.IntMap as IM
import qualified Data.Sequence as Seq

defaultUnit :: unitS -> Unit gameS teamS unitS tileS
defaultUnit unitS = Unit
    { unitState = unitS
    , unitID    = 0
    , unitTeam  = 0
    , unitType  = 0
    , unitAnimation = 0
    , unitOrders    = Seq.singleton Standby
    , unitMoveState = MoveState 
        { positionX = 0
        , positionY = 0
        , positionZ = 0
        , angle     = 0
        , speed     = 0
        , angleRate = 0
        , speedMax  = 0
        , speedRate = 0
        , weight    = 1
        , radius    = 1
        } 
    , unitValues = \_ -> []
    , unitBehaviors = IM.empty
    }

makeTeam :: Game gameS teamS unitS tileS -> teamS -> Int -> RIO ReadWrite ()
makeTeam game teamS i = defaultTeam >>= HT.write (gameTeams game) i
	where
	defaultTeam = do
	    stateRef <- Ref.make teamS
	    countRef <- Ref.make 0
	    grid     <- Grid.make (0,0) (-1)
	    noUnits  <- HT.make 10
	    return $ Team
	        { teamState = stateRef
	        , teamPlayers = []
	        , teamVision = grid
	        , teamSpawnCount = countRef
	        , teamUnits = noUnits
	        , teamValues = \_ -> []
	        , teamBehaviors = IM.empty
	        }

makeUnit :: 
	Game gameS teamS unitS tileS ->
    Unit gameS teamS unitS tileS -> 
    Int ->
    (Float,Float,Float,Float) -> 
    RIO ReadWrite ()
makeUnit game unit teamN xyza = do
	mTeam <- HT.read (gameTeams game) teamN
	case mTeam of
		Nothing -> return ()
		Just team -> do
			newUnitID <- Ref.read (teamSpawnCount team)
			Ref.modify (teamSpawnCount team) (+1)
			HT.write (teamUnits team) newUnitID 
			         (setUnitOrientation xyza unit {unitTeam = teamN})

modifyUnit ::
	Game gameS teamS unitS tileS ->
    Int -> 
    Int ->
    (Unit gameS teamS unitS tileS -> Unit gameS teamS unitS tileS) -> 
    RIO ReadWrite ()
modifyUnit game tID uID f = do
	mTeam <- HT.read (gameTeams game) tID
	case mTeam of
		Nothing -> return ()
		Just team -> do
			HT.modify (teamUnits team) uID f

setUnitPosition :: (Float,Float,Float) -> Unit gameS teamS unitS tileS -> Unit gameS teamS unitS tileS
setUnitPosition (x,y,z) u = u 
    { unitMoveState = (unitMoveState u)
        { positionX = x
        , positionY = y
        , positionZ = z
        }
    }

setUnitOrientation :: (Float,Float,Float,Float) -> Unit gameS teamS unitS tileS -> Unit gameS teamS unitS tileS
setUnitOrientation (x,y,z,a) u = u 
    { unitMoveState = (unitMoveState u)
        { positionX = x
        , positionY = y
        , positionZ = z
        , angle = a
        }
    }

setUnitFacingAngle :: Float -> Unit gameS teamS unitS tileS -> Unit gameS teamS unitS tileS
setUnitFacingAngle a u = u 
    { unitMoveState = (unitMoveState u)
        { angle = a }
    }