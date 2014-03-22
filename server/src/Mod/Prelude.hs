{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Trustworthy #-}

module Mod.Prelude where

import Data
import MIO.Privileges
import qualified MIO.Ref as Ref
import qualified MIO.Grid.Unboxed as Grid
import qualified MIO.HashTable as HT
import qualified MIO.Vector as V
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
        { msX = 0
        , msY = 0
        , msZ = 0
        , angle     = 0
        , speed     = 0
        } 
    , unitMoveStats = MoveStats
        { angleRate = 0
        , speedMax  = 0
        , speedRate = 0
        , weight    = 1
        , radius    = 1
        }
    , unitValues = \_ -> []
    , unitBehaviors = IM.empty
    }

makeTeam :: teamS -> Int -> Trainer (Game gameS teamS unitS tileS) ()
makeTeam teamS i = do
    game <- training
    stateRef <- Ref.make teamS
    countRef <- Ref.make 0
    behavRef <- Ref.make IM.empty
    grid     <- Grid.make (0,0) (-1)
    noUnits  <- HT.make 10
    let t = Team
            { teamID = i
            , teamState = stateRef
            , teamVision = grid
            , teamSpawnCount = countRef
            , teamUnits = noUnits
            , teamBehaviors = behavRef
            } 
    V.write (gameTeams game) i t

makeUnit :: 
    Unit gameS teamS unitS tileS -> 
    Int ->
    (Float,Float,Float,Float) -> 
    Trainer (Game gameS teamS unitS tileS) ()
makeUnit unit teamN xyza = do
    game <- training
    mTeam <- V.read (gameTeams game) teamN
    case mTeam of
        Nothing -> return ()
        Just team -> do
            newUnitID <- Ref.read (teamSpawnCount team)
            Ref.write (teamSpawnCount team) (newUnitID+1)
            HT.write (teamUnits team) newUnitID 
                     (setUnitOrientation xyza unit {unitID=newUnitID,unitTeam = teamN})

modifyUnit ::
    Int -> 
    Int ->
    (Unit gameS teamS unitS tileS -> Unit gameS teamS unitS tileS) -> 
    Trainer (Game gameS teamS unitS tileS) ()
modifyUnit tID uID f = do
    game <- training
    mTeam <- V.read (gameTeams game) tID
    case mTeam of
        Nothing -> return ()
        Just team -> do
            HT.modify (teamUnits team) uID f

modifyThisUnit :: 
    Unit gameS teamS unitS tileS ->
    (Unit gameS teamS unitS tileS -> Unit gameS teamS unitS tileS) -> 
    Trainer (Game gameS teamS unitS tileS) ()
modifyThisUnit u f = modifyUnit (unitTeam u) (unitID u) f

getUnitPosition :: Unit gameS teamS unitS tileS -> (Float,Float)
getUnitPosition u = let m = (unitMoveState u) in (msX m, msY m)

setUnitPosition :: (Float,Float) -> Unit gameS teamS unitS tileS -> Unit gameS teamS unitS tileS
setUnitPosition (x,y) u = u 
    { unitMoveState = (unitMoveState u)
        { msX = x
        , msY = y
        }
    }

setUnitOrientation :: (Float,Float,Float,Float) -> Unit gameS teamS unitS tileS -> Unit gameS teamS unitS tileS
setUnitOrientation (x,y,z,a) u = u 
    { unitMoveState = (unitMoveState u)
        { msX = x
        , msY = y
        , msZ = z
        , angle = a
        }
    }

setUnitFacingAngle :: Float -> Unit gameS teamS unitS tileS -> Unit gameS teamS unitS tileS
setUnitFacingAngle a u = u 
    { unitMoveState = (unitMoveState u)
        { angle = a }
    }