module Start where

import qualified Data.HashTable.IO as HT
import Control.Concurrent.ParallelIO.Global (parallel)
import Control.Monad (foldM)
import Data.Int (Int64)
import Data.Sequence ((|>),singleton)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (diffUTCTime,getCurrentTime)
import RIO.RIO
import Data
import Party

loop :: Int64 -> (a -> IO a) -> a -> IO ()
loop fps f game' = getCurrentTime >>= actualLoop 1 game'
    where
    actualLoop steps game time = do
        newGame <- f game
        timeNow <- getCurrentTime
        threadDelay $ max 0 $ fromIntegral $ 
            (steps * 1000000 `div` fps) - 
            ceiling (diffUTCTime timeNow time * fromInteger 1000000)
        actualLoop (steps + 1) newGame time 

stepGame :: Game gameS teamS unitS tileS -> IO (Game gameS teamS unitS tileS)
stepGame g = do
    let game = g {gameStep = gameStep g + 1}
    getMessages (gameParty game) >>= mapM_ (applyControlMessage game)
    gameBehavings <- fmap (map snd) $ HT.toList $ gameBehaviors game
    gameChanges   <- parallel $ map (toIO . ($game)) gameBehavings
    teams         <- fmap (map snd) $ HT.toList $ gameTeams game
    teamChanges   <- fmap concat $ parallel $ map teamCs teams
    units         <- fmap concat $ sequence $ map (fmap (map snd) . HT.toList . teamUnits) teams
    unitChanges   <- fmap concat $ parallel $ map unitCs units
    game2         <- toIO $ foldM (flip ($)) game  $ unitChanges
    game3         <- toIO $ foldM (flip ($)) game2 $ teamChanges
    toIO $ foldM (flip ($)) game3 $ gameChanges

unitCs :: Unit gameS teamS unitS tileS -> IO [Change gameS teamS unitS tileS]
unitCs u = do
    behaviors <- fmap (map snd) $ HT.toList $ unitBehaviors u
    sequence $ map (\b -> toIO $ b u) behaviors

teamCs :: Team gameS teamS unitS tileS -> IO [Change gameS teamS unitS tileS]
teamCs t = do
    behaviors <- fmap (map snd) $ HT.toList $ teamBehaviors t
    sequence $ map (\b -> toIO $ b t) behaviors

applyControlMessage :: Game gameS teamS unitS tileS -> (Player, ControlMessage) -> IO ()
applyControlMessage game (player, OrderMsg (Orders shift ids order)) = do
    maybeTeam <- HT.lookup teams teamID
    case maybeTeam of
        Just team -> do
            let units = teamUnits team
            flip mapM_ ids $ \unit_id -> do
                maybeUnit <- HT.lookup units unit_id
                case maybeUnit of
                    Just unit -> 
                        if shift
                        then HT.insert units unit_id $ unit {unitOrders = unitOrders unit |> order}
                        else HT.insert units unit_id $ unit {unitOrders = singleton order}
                    Nothing -> return ()
        Nothing -> return ()
    where
    teams = gameTeams game
    teamID = playerTeam player
applyControlMessage _ _ = return ()