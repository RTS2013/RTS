module Mod.Setup
( runMod
, defaultGameState
, defaultTileState
, defaultTeamState
) where

-- Standard Modules
import qualified Data.IntMap      as IM
-- Custom Modules
import qualified MIO.HashTable    as HT
import qualified MIO.Ref          as Ref
import MIO.Random
import qualified MIO.Grid.Unboxed as GUM -- Grid Unboxed Mutable
import qualified MIO.Grid.Boxed   as GBM -- Grid Boxed Mutable
import qualified Grid.Unboxed     as GU  -- Grid Unboxed Immutable
import qualified Grid.Boxed       as GB  -- Grid Boxed Immutable
import qualified Mod.Pathing      as P
import qualified Movers           as Move
import qualified KDT              as KDT

import Data.Sequence (viewl,ViewL(..))
import MIO.Privileges
import Mod.Prelude

import Mod.Data
import Data

{-
DD = damage dealt
DT = damage taken

(DD + DT)*0.5 + sqrt( (DD*a) * (DT*b)
where a + b = 1
-}

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

runMod :: Int -> ModTrainer ()
runMod nTeams = do
    rng <- getMIOGen
    flip evalRandT rng $ do
        flip mapM_ [0..nTeams-1] $ \nTeam -> do
            flip mapM_ [0..20::Int] $ \_ -> do
                x <- getRandomR (0,1024)
                y <- getRandomR (0,1024)
                a <- getRandomR (0,2*pi)
                lift $ makeUnit (defaultUnit defaultUnitState) nTeam (x,y,0,a)
    return ()

followOrders :: ModTrainer UnitBehavior
followOrders = do
    game <- training
    return $ do
        u <- behaving
        case viewl (unitOrders u) of
            EmptyL -> return $ return ()
            (a:<_) -> case a of
                (Move (Point x y z)) -> 
                                    if null $ unitPath $ unitState u 
                                    then undefined
                                    else undefined
                _ -> return $ return ()


















{-
handleOrders :: Trainer ModGame ()
handleOrders u = do
    case viewl (unitOrders u) of
            EmptyL -> return $ return ()
            (a:<_) -> case a of
                (Move (Point x y z)) -> undefined -- handleMoveOrder g u
                _ -> return $ return ()


handleMoveOrder :: ModGame -> ModBehavior Unit
handleMoveOrder g u = do
    let unitS = unitState u
    if undefined --calcPath unitS
    then do
        kdt <- Ref.read (gameKDT g)
        let (x,y) = getUnitPosition u
        let nearby = KDT.nearby kdt [msX . unitMoveState, msY . unitMoveState] [x,y] (radius $ unitMoveStats u)
        --let xy = Move.move nearby maxRadius 
        let xy' = undefined
        return $ modifyThisUnit u $ setUnitPosition xy'
    else undefined

firstUnit :: ModGame -> ModUnit
firstUnit game = (genericGroundUnit game)
    { unitBehaviors = IM.singleton 0 $ \u -> return $ do
        case viewl (unitOrders u) of
            EmptyL -> return ()
            (a:<_) -> case a of
                (Move (Point x y z)) -> 
                    if calcPath $ unitState u 
                    then getPath (snd . G.read (gameTiles game)) 
                _ -> return ()
    }

maxRadius :: Float
maxRadius = 1.0


moveDistToPoint :: ModGame -> Float -> Float -> Float -> ModBehavior Unit
moveDistToPoint g dx dy dist u = do
    kdt <- Ref.read (gameKDT g)
    return $ do
        modifyUnit g (unitTeam u) (unitID u) $
            let (nx,ny) = M.move
                    (\(x,y) r -> map xyrw $ KDT.inRange 
                        kdt 
                        [ msX . unitMoveState, msY . unitMoveState ] 
                        [x,y] 
                        r)
                    maxRadius
                    (dx,dy)
                    dist
                    (xyrw u)
                    in
            setUnitOrientation ( nx
                               , ny
                               , msZ $ unitMoveState u
                               , atan2 (dx - msX (unitMoveState u)) (dy - msY (unitMoveState u))
                               )
                    
    where
    xyrw Unit {unitMoveState=m,unitMoveStats=s} = 
        let MoveState {msX=x,msY=y} = m 
            MoveStats {radius=r,weight=w} = s 
        in (x,y,r,w)
-}