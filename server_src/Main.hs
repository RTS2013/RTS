module Main where

import qualified Local.Matrices.UnboxedMatrix2D as M
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector         as V
import qualified Data.IntMap.Strict  as IM
import qualified Local.KDT           as KDT
import Control.Monad.Primitive (PrimState)
import Control.DeepSeq (NFData)
import Data.Word (Word8,Word16)
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)
import GameNetwork (gameServer)
import Control.Monad.Par (runPar,spawn,get)
import Data.Traversable (traverse)
import Control.Parallel.Strategies (parMap,rdeepseq)
import Control.Monad.ST (runST)
import Data.Time.Clock 
    ( UTCTime
    , NominalDiffTime
    , diffUTCTime
    , getCurrentTime
    , picosecondsToDiffTime )
import Data

main = do 
    -- teamCounts <- fmap (read . (!! 0)) getArgs
    putStrLn "Enter the number of players per team (ex. [1,3,2] )"
    teamCounts <- fmap read getLine
    players <- gameServer $ UV.fromList teamCounts
    m <- M.make 1024 1024 (0 :: Word16)
    let units = undefined
    let teams = V.fromList $ map (\n -> Team n IM.empty m []) [0..(length teamCounts - 1)]
    let world = World teams KDT.empty m
    -- Start main loop
    loop 10 stepGame world
    return () 

loop :: Integer -> (a -> IO a) -> a -> IO ()
loop fps f world = getCurrentTime >>= actualLoop 1 world
    where
    actualLoop steps world time = do
        newWorld <- f world
        timeNow <- getCurrentTime
        threadDelay $ max 0 $ fromIntegral $ 
            (steps * 1000000 `div` fps) - 
            ceiling (diffUTCTime timeNow time * fromInteger 1000000)
        actualLoop (steps + 1) newWorld time 

stepGame :: World -> IO World
stepGame w = do 
    -- Setup KDT
    let w = w { world_kdt = KDT.makePar [moveState_x . actor_moveState, moveState_y . actor_moveState] . 
              concat . V.toList . V.map (IM.elems . team_entities) $ world_teams w }
    -- Gather effects
    let (worldEffects,teamEffects,actorEffects) = sortEffects (V.length $ world_teams w) $ concat 
                        $ map (\t -> collectEffects w (team_entities t)) $ V.toList (world_teams w)
    -- Apply world effects
    let w = applyWorldEffects w worldEffects
    -- Apply team effects
    let w = w {
                world_teams = V.imap (\i t -> applyTeamEffects t (teamEffects V.! i)) (world_teams w)
            }
    -- Apply actor effects
    let w = w {
                world_teams = V.imap (\i t -> t {
                    team_entities = applyActorEffects (team_entities t) $ actorEffects V.! i
                }) $ world_teams w
            }
    -- Apply self effects
    let w = w {
                world_teams = V.imap (\i t -> t {
                    team_entities = parMapIntMap (applySelfEffects w) $ team_entities t
                }) $ world_teams w
            }
    return w

-- Helper functions for stepGame 
{-# SPECIALIZE parMapIntMap :: (Actor -> Actor) -> IM.IntMap Actor -> IM.IntMap Actor #-}
parMapIntMap :: (NFData b) => (a -> b) -> IM.IntMap a -> IM.IntMap b
parMapIntMap f im = runPar $ traverse (\a -> spawn (return $ f a)) im >>= traverse get

applySelfEffects :: World -> Actor -> Actor
applySelfEffects w a = IM.foldl (\a f -> f w a) a (identity_selfEffects $ actor_identity a)

collectEffects :: World -> IM.IntMap Actor -> [Effect]
collectEffects w im = concat $ parMap rdeepseq 
    (\a -> concat $ map (\f -> f w a) (IM.elems $ identity_effects $ actor_identity a)) $ IM.elems im

-- Puts list of effects into their own categories distributed to each team (except world effects)
sortEffects :: Int -> [Effect] -> ([World -> World], V.Vector [Team -> Team], V.Vector [(Int, Actor -> Maybe Actor)])
sortEffects numTeams = teamEffects
    where
    teamEffects xs = runST $ do
        team <- MV.replicate numTeams []
        act <- MV.replicate numTeams []
        sortCons xs [] team act
    sortCons ((ActorEffect t i f):xs) world team act = do (modif act t ((i,f):)) >> sortCons xs world team act
    sortCons ((TeamEffect t f):xs) world team act = do (modif team t (f:)) >> sortCons xs world team act
    sortCons ((WorldEffect f):xs) world team act = sortCons xs (f:world) team act 
    sortCons _ world team act = do 
        frozenTeam <- V.unsafeFreeze team 
        frozenAct  <- V.unsafeFreeze act
        return (world,frozenTeam,frozenAct)

    modif v i f = MV.read v i >>= MV.write v i . f

applyWorldEffects :: World -> [World -> World] -> World
applyWorldEffects w xs = foldr (\f w -> f w) w xs

applyTeamEffects :: Team -> [Team -> Team] -> Team
applyTeamEffects w xs = foldr (\f w -> f w) w xs

applyActorEffects :: IM.IntMap Actor -> [(Int, Actor -> Maybe Actor)] -> IM.IntMap Actor
applyActorEffects im xs = foldr (\(i,f) im -> IM.update f i im) im xs 

handleClientMessage :: ClientMessage -> IO [Effect]
handleClientMessage w (ActorMessage team 0 shift x y actors) = do
    
    return w