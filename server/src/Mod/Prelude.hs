{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Trustworthy, BangPatterns #-}

module Mod.Prelude where

import           Control.Monad.Identity (runIdentity)
import qualified Data.Vector.Unboxed as UV
import qualified Grid.Boxed as GB
import qualified Grid.Unboxed as GU
import           MIO.Privileges (changing,behaving,Change,Behavior)
import qualified MIO.HashTable as HT
import qualified MIO.Ref as Ref
import qualified MIO.Vector as V
import qualified Data.IntMap as IM
import qualified Data.Sequence as Seq
import qualified Tree2D as KDT
import Data
import Pathfinding
import Movers (wallStop,move)

type TimeDelta = Double

type PChange g u t = Change (Game g u t) ()
type PBehavior g u t = Behavior (Game g u t, TimeDelta, Unit g u t) (Change (Game g u t) ())

{-# INLINE defaultUnit #-}
defaultUnit :: u -> Unit g u t
defaultUnit unitS = Unit
    { unitModState = unitS
    , unitAnimation = 0
    , unitOrders = Seq.singleton Standby
    , unitMoveState = Vec4 0 0 0 0
    , unitStaticState = StaticState 0 0 0 2 2 1 1 (\_ -> [])
    , unitBehaviors = IM.empty
    }

{-# INLINE makeUnit #-}
makeUnit :: 
    Unit g u t -> 
    Int ->
    (Double,Double,Double,Double) -> 
    Change (Game g u t) Int
makeUnit unit teamN xyza = do
    game <- changing
    mTeam <- V.read (gameTeamsVec game) teamN
    case mTeam of
        Nothing -> fail "You suck"
        Just team -> do
            newUnitID <- Ref.read (teamSpawnCount team)
            Ref.write (teamSpawnCount team) (newUnitID+1)
            HT.write (teamUnits team) newUnitID 
                     (css (setUnitOrientation xyza unit) $ \s -> s {unitID=newUnitID,unitTeam=teamN})
            return newUnitID
    where
    -- Change static state
    css u f = u {unitStaticState = f $ unitStaticState u}

{-# INLINE changeSpecificUnit #-}
changeSpecificUnit :: Int -> Int -> (Unit g u t -> Unit g u t) -> PChange g u t
changeSpecificUnit tID uID f = do
    game <- changing
    mTeam <- V.read (gameTeamsVec game) tID
    case mTeam of
        Nothing -> return ()
        Just team -> HT.modify (teamUnits team) uID f

{-# INLINE changeUnit #-}
changeUnit :: Unit g u t -> (Unit g u t -> Unit g u t) -> PChange g u t
changeUnit u f = do
    game <- changing
    mTeam <- V.read (gameTeamsVec game) (unitTeam $ unitStaticState u)
    case mTeam of
        Nothing -> return ()
        Just team -> HT.modify (teamUnits team) (unitID $ unitStaticState u) f

{-# INLINE getUnitPosition #-}
getUnitPosition :: Unit g u t -> (Double,Double)
getUnitPosition u = let m = (unitMoveState u) in (_v1 m, _v2 m)

{-# INLINE setUnitPosition #-}
setUnitPosition :: (Double,Double) -> Unit g u t -> Unit g u t
setUnitPosition (x,y) u = u 
    { unitMoveState = (unitMoveState u)
        { _v1 = x
        , _v2 = y
        }
    }

{-# INLINE setUnitOrientation #-}
setUnitOrientation :: (Double,Double,Double,Double) -> Unit g u t -> Unit g u t
setUnitOrientation (x,y,z,a) u = u 
    { unitMoveState = (unitMoveState u)
        { _v1 = x
        , _v2 = y
        , _v3 = z
        , _v4 = a
        }
    }

{-# INLINE setUnitFacingAngle #-}
setUnitFacingAngle :: Double -> Unit g u t -> Unit g u t
setUnitFacingAngle a u = u 
    { unitMoveState = (unitMoveState u)
        { _v4 = a }
    }

{-# INLINE handleOrdersBehavior #-}
handleOrdersBehavior :: PBehavior g u t
handleOrdersBehavior = do
    (_,_,unit) <- behaving
    case Seq.viewl (unitOrders unit) of
        Seq.EmptyL -> let (x,y) = getUnitPosition unit in groundMoveBehavior x y [(x,y)]
        (a Seq.:<_) -> case a of
            Move x y path -> groundMoveBehavior x y path
            _ -> let (x,y) = getUnitPosition unit in groundMoveBehavior x y [(x,y)]


{-# INLINE groundMoveBehavior #-}
groundMoveBehavior :: Double -> Double -> [(Double,Double)] -> PBehavior g u t
groundMoveBehavior x y _ = do
    (game,td,unit) <- behaving
    let unitToStats u = let (ux,uy) = getUnitPosition u in (ux,uy,radius $! unitStaticState u, weight $! unitStaticState u)
    let (!ux,!uy,!ur,!uw) = unitToStats unit
    pathing <- Ref.read (gamePathing game)
    kdt <- Ref.read (gameKDTRef game)
    let newPath = maybe [(floor x, floor y)] id $! getPath
            pathing
            (floor ux, floor uy)
            (floor x, floor y)
    let moveXY dx dy = runIdentity 
                    $! wallStop (\(tx,ty) -> return . maybe False id $! isTileOpen pathing (floor tx) (floor ty))
                                (ux,uy,ur) 
                    $! move (ux,uy,ur,uw) (dx,dy) 
                            (td * speed (unitStaticState unit))
                            (map unitToStats $ KDT.inRange kdt (ux,uy,ur))
    case newPath of
        (dx,dy):_ -> do
            let (nx,ny) = moveXY (fromIntegral dx) (fromIntegral dy)
            dx `seq` dy `seq` nx `seq` ny `seq` return $!
                changeUnit unit (setUnitFacingAngle (atan2 (ny-uy) (nx-ux)) . setUnitPosition (nx,ny))
        _ -> do
            let (nx,ny) = moveXY x y
            nx `seq` ny `seq`
                return $! changeUnit unit (setUnitFacingAngle (atan2 (ny-uy) (nx-ux)) . setUnitPosition (nx,ny))