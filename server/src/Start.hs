{-# LANGUAGE Trustworthy #-}

module Start where

import qualified Data.HashTable.IO as HT
import qualified Data.IntMap       as IM
import Data.Monoid ((<>))
import Data.Binary (encode)
import Data.IORef
import Control.Concurrent.ParallelIO.Global (parallel)
import Data.Int (Int64)
import Data.Sequence ((|>),singleton)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (diffUTCTime,getCurrentTime)
import Data.Word (Word8)
import qualified Grid.UnboxedGrid as Grid
import RIO.RIO
import RIO.Privileges
import Data
import Party
import safe Mod (runMod)

main :: IO ()
main = do
    teamNums <- fmap read getLine
    party    <- openDoors 4444 teamNums :: IO (Party ControlMessage)
    game     <- defaultGame party >>= toIO . runMod (length teamNums)
    loop 10 stepGame game

defaultGame :: Party ControlMessage -> IO (Game () teamS unitS ())
defaultGame party = do 
    stepRef  <- newIORef 0
    stateRef <- newIORef ()
    teams <- HT.new
    grid  <- Grid.make (0,0) ()
    valuesRef <- newIORef $ \_ -> []
    behaveRef <- newIORef IM.empty
    return $ Game 
        { gameStep = stepRef
        , gameState = stateRef 
        , gameParty = party
        , gameTeams = teams
        , gameTiles = grid
        , gameValues = valuesRef
        , gameBehaviors = behaveRef
        }

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
stepGame game = do
    stepN <- readIORef (gameStep game)
    modifyIORef (gameStep game) (+1)
    getMessages (gameParty game) >>= mapM_ (applyControlMessage game)
    gameBehavings <- fmap IM.elems $ readIORef (gameBehaviors game)
    gameCs        <- parallel $ map (toIO . ($game)) gameBehavings
    teams         <- fmap (map snd) $ HT.toList $ gameTeams game
    teamCs        <- fmap concat $ parallel $ map teamChanges teams
    units         <- fmap concat $ sequence $ map (fmap (map snd) . HT.toList . teamUnits) teams
    unitCs        <- fmap concat $ parallel $ map unitChanges units
    toIO $ sequence_ unitCs >> sequence_ teamCs >> sequence_ gameCs
    allUnits <- fmap concat $ sequence $ map (fmap (map snd) . HT.toList . teamUnits) teams
    players  <- allPlayers (gameParty game)
    -- Send unit information to all players
    sendToPlayers (encode stepN <> encode (0::Word8)) allUnits players
    return game

unitChanges :: Unit gameS teamS unitS tileS -> IO [RIO ReadWrite ()]
unitChanges u = do
    let behaviors = IM.elems $ unitBehaviors u
    sequence $ map (\b -> toIO $ b u) behaviors

teamChanges :: Team gameS teamS unitS tileS -> IO [RIO ReadWrite ()]
teamChanges t = do
    let behaviors = IM.elems $ teamBehaviors t
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