{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Mod.Setup
( runMod
, defaultGameState
, defaultNodeState
, defaultTeamState
) where

import Control.DeepSeq (deepseq)
import qualified Control.Concurrent.Async as Async
import qualified Data.Array.Repa.Repr.Vector as R
import qualified Data.Array.Repa          as R
import qualified Data.IntMap              as IM
import qualified Data.Vector.Unboxed      as VU
import qualified MIO.HashTable            as HT
import qualified MIO.Ref                  as Ref
import qualified MIO.Grid.Unboxed         as GUM -- Grid Unboxed Mutable
import qualified MIO.Grid.Boxed           as GBM -- Grid Boxed Mutable
import qualified Grid.Unboxed             as GU  -- Grid Unboxed Immutable
import qualified Grid.Boxed               as GB  -- Grid Boxed Immutable
import qualified Movers                   as Move
import qualified Tree2D                   as KDT
import Data
import Mod.Data
import Mod.Prelude
import MIO.Privileges
import MIO.Random
import qualified MIO.Repa as Repa
import Pathfinding
import Terrain

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

width :: Int
width = 128

height :: Int
height = 128

dims :: (Int,Int)
dims = (width,height)

runMod :: Int -> ModChange ()
runMod nTeams = do
    game <- changing
    let grid = terrainCliffs dims $! terrainNoise 0 dims
    let pathing = setup (\xy -> maybe False snd $! flip GU.read xy grid) dims
    Ref.write (gamePathing game) $! pathing
    spawnUnits nTeams

spawnUnits :: Int -> ModChange ()
spawnUnits nTeams = do
    rng <- newMIOGen
    flip evalRandT rng $! do
        flip mapM_ [0..nTeams-1] $! \nTeam -> do
            flip mapM_ [0..99::Int] $! \_ -> do
                x <- getRandomR (0,fromIntegral width-1)
                y <- getRandomR (0,fromIntegral width-1)
                a <- getRandomR (0,2*pi)
                lift $! makeUnit ((defaultUnit defaultUnitState) {unitBehaviors = IM.singleton 0 handleOrdersBehavior}) nTeam (x,y,0,a)

terrainNoise :: Int -> (Int,Int) -> GU.Grid (Int,Bool)
terrainNoise mapRN (w,h) =
    let arr = randomPerlin mapRN 0 (0,0) (w,h) (rangeMap (2::Int,True) [(-0.9,(0,False)),(0.25,(1,True))]) in
    GU.fromVector (w,h) $! R.toUnboxed arr

terrainCliffs :: (Int,Int) -> GU.Grid (Int,Bool) -> GU.Grid (Int,Bool)
terrainCliffs (w,h) grid =
    GU.update grid $! map (\(x,y) -> (x, y, arr R.! R.ix2 x y)) cliffs
    where
    arr = R.fromUnboxed (R.ix2 w h) $! GU.toVector grid
    cliffs = filter (isCliff arr) [(x,y) | x <- [0..w-1], y <- [0..h-1]]
    isCliff tiles (x,y) = any ((>(fst $ tiles R.! (R.ix2 x y)))) 
        $ map (\(x',y') -> fst $! tiles R.! (R.ix2 x' y')) 
            [ (a,b) 
            | a <- [x-1,x+1]
            , b <- [y-1..y+1]
            , a /= x && b /= y
            , a >= 0 && a < w
            , b >= 0 && b < h
            ]