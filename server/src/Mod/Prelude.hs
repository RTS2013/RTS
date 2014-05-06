{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Trustworthy, BangPatterns #-}

module Mod.Prelude where

import           Control.Monad.Identity (runIdentity)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as UV
import qualified Grid.Boxed as GB
import qualified Grid.Unboxed as GU
import           MIO.Privileges (changing,behaving,Change,Behavior)
import qualified MIO.HashTable as HT
import qualified MIO.Ref as Ref
import qualified MIO.Vector as V
import qualified Data.IntMap as IM
import qualified Data.Sequence as Seq
import qualified KDTree as KDT
import Data
import Pathing
import Movers (wallStop,move)

type TimeDelta = Float

type PChange g u t = Change (Game g u t) ()
type PBehavior g u t = Behavior (Game g u t, TimeDelta, Unit g u t) (Change (Game g u t) ())

defaultUnit :: u -> Unit g u t
defaultUnit unitS = Unit
    { unitModState = unitS
    , unitAnimation = 0
    , unitOrders = Seq.singleton Standby
    , unitMoveState = Vec4 0 0 0 0
    , unitStaticState = StaticState 0 0 0 1 1 1 1 (\_ -> [])
    , unitBehaviors = IM.empty
    }

makeUnit :: 
    Unit g u t -> 
    Int ->
    (Float,Float,Float,Float) -> 
    Change (Game g u t) ()
makeUnit unit teamN xyza = do
    game <- changing
    mTeam <- V.read (gameTeamsVec game) teamN
    case mTeam of
        Nothing -> return ()
        Just team -> do
            newUnitID <- Ref.read (teamSpawnCount team)
            Ref.write (teamSpawnCount team) (newUnitID+1)
            HT.write (teamUnits team) newUnitID 
                     (css (setUnitOrientation xyza unit) $ \s -> s {unitID=newUnitID,unitTeam=teamN})
    where
    -- Change static state
    css u f = u {unitStaticState = f $ unitStaticState u}

changeSpecificUnit :: Int -> Int -> (Unit g u t -> Unit g u t) -> PChange g u t
changeSpecificUnit tID uID f = do
    game <- changing
    mTeam <- V.read (gameTeamsVec game) tID
    case mTeam of
        Nothing -> return ()
        Just team -> HT.modify (teamUnits team) uID f

changeUnit :: Unit g u t -> (Unit g u t -> Unit g u t) -> PChange g u t
changeUnit u f = do
    game <- changing
    mTeam <- V.read (gameTeamsVec game) (unitTeam $ unitStaticState u)
    case mTeam of
        Nothing -> return ()
        Just team -> HT.modify (teamUnits team) (unitID $ unitStaticState u) f

getUnitPosition :: Unit g u t -> (Float,Float)
getUnitPosition u = let m = (unitMoveState u) in (_v1 m, _v2 m)

setUnitPosition :: (Float,Float) -> Unit g u t -> Unit g u t
setUnitPosition (x,y) u = u 
    { unitMoveState = (unitMoveState u)
        { _v1 = x
        , _v2 = y
        }
    }

setUnitOrientation :: (Float,Float,Float,Float) -> Unit g u t -> Unit g u t
setUnitOrientation (x,y,z,a) u = u 
    { unitMoveState = (unitMoveState u)
        { _v1 = x
        , _v2 = y
        , _v3 = z
        , _v4 = a
        }
    }

setUnitFacingAngle :: Float -> Unit g u t -> Unit g u t
setUnitFacingAngle a u = u 
    { unitMoveState = (unitMoveState u)
        { _v4 = a }
    }

handleOrdersBehavior :: PBehavior g u t
handleOrdersBehavior = do
    (_,_,unit) <- behaving
    case Seq.viewl (unitOrders unit) of
        Seq.EmptyL -> return $ return ()
        (a Seq.:<_) -> case a of
            Move x y path -> groundMoveBehavior x y path
            _ -> return $ return ()

groundMoveBehavior :: Float -> Float -> [(Float,Float)] -> PBehavior g u t
groundMoveBehavior x y path = do
    (game,td,unit) <- behaving
    if null path
    then do
        let unitToStats u = let (ux,uy) = getUnitPosition u in (ux,uy,radius $ unitStaticState u, weight $ unitStaticState u)
        let (!ux,!uy,!ur,!uw) = unitToStats unit
        tiles <- Ref.read (gameTilesRef game)
        corners <- Ref.read (gameCornersRef game)
        kdt <- Ref.read (gameKDTRef game)
        let !newPath = maybe [(floor x, floor y)] id $ getPath
                (\xy -> maybe False snd $ GU.read tiles xy)
                (maybe UV.empty id . GB.read corners)
                (floor ux, floor uy)
                (floor x, floor y)
        let moveXY dx dy = runIdentity . wallStop 
                (\(tx,ty) -> return . maybe False snd $ GU.read tiles (floor tx, floor ty))
                (ux,uy,ur) $ move (ux,uy,ur,uw) (dx,dy) 
                                  (td * speed (unitStaticState unit))
                                  (map unitToStats . Set.toList $ KDT.nearby kdt ur [ux,uy])
        case newPath of
            (dx,dy):_ -> do
                let (!nx,!ny) = moveXY (fromIntegral dx) (fromIntegral dy)
                return $ do
                    changeUnit unit (setUnitFacingAngle (atan2 (ny-uy) (nx-ux)) . setUnitPosition (nx,ny))
            _ -> return $ return ()
    else return $ return ()