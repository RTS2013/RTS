module Main where

import qualified Grid.UnboxedGrid as G
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector         as V
import qualified Data.IntMap.Strict  as IM
import qualified Data.Sequence       as S
import qualified Local.KDT           as KDT
import Control.Concurrent.STM.TVar
import Control.Parallel.Strategies (parMap,rdeepseq)
import Control.Monad.ST (runST)
import Control.Monad.Primitive (PrimState)
import Control.DeepSeq (NFData)
import Data.Int (Int64)
import Data.Word (Word8,Word16)
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)
import RTSNetwork (connectPlayers,serverCommand)
import Control.Monad.Par (runPar,spawn,get)
import Data.Traversable (traverse)
import Data.Time.Clock 
    ( diffUTCTime
    , getCurrentTime )
import Movement
import Data

loop :: Int64 -> (Int64 -> a -> IO a) -> a -> IO ()
loop fps f game = getCurrentTime >>= actualLoop 1 game
    where
    actualLoop steps game time = do
        newGame <- f steps game
        timeNow <- getCurrentTime
        threadDelay $ max 0 $ fromIntegral $ 
            (steps * 1000000 `div` fps) - 
            ceiling (diffUTCTime timeNow time * fromInteger 1000000)
        actualLoop (steps + 1) newGame time 

setupGame = do
    --teams <- fmap (read . (!! 0)) getArgs
    putStrLn "Enter players per team (Ex.  [1,3,2]  )."
    teams        <- fmap (UV.fromList . (read :: String -> [Int])) getLine
    teamsVar     <- newTVarIO teams
    playersVar   <- newTVarIO []
    messagesVar  <- newTVarIO [] :: IO (TVar [()])
    serverThread <- connectPlayers teamsVar playersVar messagesVar
    putStrLn "Type '.start' to start the game. Type '.?' for more commands."
    serverCommand teamsVar playersVar messagesVar

step :: Int64 -> Game gameS teamS unitS tileS -> IO (Game gameS teamS unitS tileS)
step steps game = do
    let gameS = gameState game
    let unitKDT = makeUnitKDT game
    game <- return game
    return game
    where
    makeUnitKDT game = 
        KDT.makePar [xCoord . unitPoint, yCoord . unitPoint] . 
        concat . V.toList . V.map (IM.elems . teamActors) $ gameTeams game

-- Helper functions for stepGame 
parMapIntMap :: (NFData b) => (a -> b) -> IM.IntMap a -> IM.IntMap b
parMapIntMap f im = runPar $ traverse (\a -> spawn (return $ f a)) im >>= traverse get

applySelfEffects :: Game gameS teamS unitS tileS -> Unit gameS teamS unitS tileS -> Unit gameS teamS unitS tileS
applySelfEffects w a = IM.foldl (\a f -> f w a) a (unitSelfEffects $ identity a)

collectEffects :: Game gameS teamS unitS tileS -> IM.IntMap Unit gameS teamS unitS tileS -> [Effect]
collectEffects w im = concat $ parMap rdeepseq 
    (\a -> concat $ map (\f -> f w a) (IM.elems $ unitEffects $ identity a)) $ IM.elems im

-- Puts list of effects into their own categories distributed to each team (except game effects)
sortEffects :: Int -> [Effect] -> 
    ([Game gameS teamS unitS tileS -> Game gameS teamS unitS tileS], V.Vector [Team -> Team], V.Vector [(Int, Unit gameS teamS unitS tileS -> Maybe Unit gameS teamS unitS tileS)])
sortEffects numTeams = teamEffects
    where
    teamEffects xs = runST $ do
        team <- MV.replicate numTeams []
        act <- MV.replicate numTeams []
        sortCons xs [] team act
    sortCons ((ActorEffect t i f):xs) game team act = do (modif act t ((i,f):)) >> sortCons xs game team act
    sortCons ((TeamEffect t f):xs) game team act = do (modif team t (f:)) >> sortCons xs game team act
    sortCons ((WorldEffect f):xs) game team act = sortCons xs (f:game) team act 
    sortCons _ game team act = do 
        frozenTeam <- V.unsafeFreeze team 
        frozenAct  <- V.unsafeFreeze act
        return (game,frozenTeam,frozenAct)
    modif v i f = MV.read v i >>= MV.write v i . f

applyWorldEffects :: Game gameS teamS unitS tileS -> [Game gameS teamS unitS tileS -> Game gameS teamS unitS tileS] -> Game gameS teamS unitS tileS
applyWorldEffects w xs = foldr (\f w -> f w) w xs

applyTeamEffects :: Team -> [Team -> Team] -> Team
applyTeamEffects w xs = foldr (\f w -> f w) w xs

applyActorEffects :: IM.IntMap Unit gameS teamS unitS tileS -> [(Int, Unit gameS teamS unitS tileS -> Maybe Unit gameS teamS unitS tileS)] -> IM.IntMap Unit gameS teamS unitS tileS
applyActorEffects im xs = foldr (\(i,f) im -> IM.update f i im) im xs 

handleClientMessage :: ClientMessage -> IO [Effect]
handleClientMessage (ActorMessage team 0 shift x y units) = do
    return $ map (\actId -> ActorEffect team actId addMove) units
    where
    addMove :: Unit gameS teamS unitS tileS -> Maybe Unit gameS teamS unitS tileS
    addMove a = 
        let atts = (attributes a)
            ords = orders atts in
        Just $
        if shift
        then a {attributes = atts {orders = ords S.|> Move x y}} 
        else a {attributes = atts {orders = S.singleton $ Move x y}} 