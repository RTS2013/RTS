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
import qualified KDTree                   as KDT
import Data
import Mod.Data
import Mod.Prelude
import MIO.Pathing
import MIO.Privileges
import MIO.Random
import qualified MIO.Repa as Repa
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
width = 1024

height :: Int
height = 1024

dims :: (Int,Int)
dims = (width,height)

runMod :: Int -> ModChange ()
runMod nTeams = do
    game <- changing
    terrainNoise dims
    terrainCliffs dims
    pathing <- do
        tiles <- Ref.read (gameTilesRef game)
        arr <- setGroupsPIO dims (\xy -> maybe False snd $! flip GU.read xy tiles)
        let vec = R.toVector arr
        deepseq vec (return $! GB.fromVector dims vec)
    Ref.write (gameCornersRef game) $! pathing
    spawnUnits nTeams

spawnUnits :: Int -> ModChange ()
spawnUnits nTeams = do
    spawnRNG <- newMIOGen
    flip evalRandT spawnRNG $! do
        flip mapM_ [0..nTeams-1] $! \nTeam -> do
            flip mapM_ [0..99::Int] $! \_ -> do
                x <- getRandomR (0,fromIntegral width-1)
                y <- getRandomR (0,fromIntegral width-1)
                a <- getRandomR (0,2*pi)
                lift $! makeUnit ((defaultUnit defaultUnitState) {unitBehaviors = IM.singleton 0 handleOrdersBehavior}) nTeam (x,y,0,a)

terrainNoise :: (Int,Int) -> ModChange ()
terrainNoise (w,h) = do
    mapRNG <- fmap (fst . random) newMIOGen
    let arr = randomPerlin mapRNG 0 (0,0) (w,h) (rangeMap (2::Int,True) [(-0.9,(0,False)),(0.25,(1,True))])
    game <- changing
    Ref.write (gameTilesRef game) (GU.fromVector (w,h) $! R.toUnboxed arr)
    return ()

terrainCliffs :: (Int,Int) -> ModChange ()
terrainCliffs (w,h) = do
    game <- changing
    tiles <- fmap (R.fromUnboxed (R.ix2 w h) . GU.toVector) $! Ref.read (gameTilesRef game)
    let cliffs = filter (isCliff tiles) [(x,y) | x <- [0..w-1], y <- [0..h-1]]
    outlines <- fmap (\g -> GU.fromVector (w,h) $! GU.toVector g VU.// map (\(x,y) -> (w * y + x, tiles R.! R.ix2 x y)) cliffs) 
        $! Ref.read (gameTilesRef game)
    Ref.write (gameTilesRef game) outlines
    where
    isCliff tiles (x,y) = any ((>(fst $ tiles R.! (R.ix2 x y)))) 
        $ map (\(x',y') -> fst $! tiles R.! (R.ix2 x' y')) 
            [(a,b) | a <- [x-1,x+1]
            , b <- [y-1..y+1]
            , a /= x && b /= y
            , a >= 0 && a < w
            , b >= 0 && b < h
            ]