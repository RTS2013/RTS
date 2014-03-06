{-# LANGUAGE Trustworthy #-}

module Start where

import qualified Data.HashTable.IO as HT
import qualified Data.IntMap       as IM
import qualified Grid.UnboxedGrid  as Grid
import Data.Monoid ((<>))
import Data.Binary (encode)
import Data.IORef
import Control.Concurrent.ParallelIO.Global (parallel,stopGlobalPool)
import Data.Int (Int64)
import Data.Sequence ((|>),singleton)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (diffUTCTime,getCurrentTime)
import Data.Word (Word8)
import RIO.RIO
import RIO.Privileges
import RIO.Prelude (makeTeam)
import Data
import Party
import safe Mod (runMod,defaultGameState,defaultTileState,defaultTeamState)

main :: IO ()
main = do
    teamNums <- fmap read getLine
    party    <- openDoors 4444 teamNums :: IO (Party ControlMessage)
    defaultGame <- do 
        stepRef  <- newIORef 0
        stateRef <- newIORef defaultGameState
        teams <- HT.new
        grid  <- Grid.make (1024,1024) defaultTileState
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
    -- Add default teams to game
    toIO $ mapM_ (makeTeam defaultGame defaultTeamState) [0..length teamNums - 1]
    -- Run Mod on game
    game <- toIO $ runMod (length teamNums) defaultGame
    -- Start game
    loop 10 stepGame game
    stopGlobalPool

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
    -- Apply player commands to units
    getMessages (gameParty game) >>= mapM_ (applyControlMessage game)
    -- Gather game behaviors
    gameBehavings <- fmap IM.elems $ readIORef (gameBehaviors game)
    -- Gather all teams
    teams <- fmap (map snd) $ HT.toList $ gameTeams game
    -- Gather all units
    units <- fmap concat $ sequence $ map (fmap (map snd) . HT.toList . teamUnits) teams
    -- Gather game changes
    gamesChanges <- parallel (map (toIO . ($game)) gameBehavings)
    -- Gather team changes
    teamsChanges <- parallel (map teamChanges teams)
    -- Gather unit changes
    unitsChanges <- parallel (map unitChanges units)
    -- Apply game changes
    toIO $ sequence_ gamesChanges
    -- Apply team changes
    toIO $ mapM_ sequence_ teamsChanges
    -- Apply unit changes
    toIO $ mapM_ sequence_ unitsChanges
    -- Extract all units from all teams
    allUnits <- fmap concat $ sequence $ map (fmap (map snd) . HT.toList . teamUnits) teams
    -- Send unit all information to all players
    players <- allPlayers (gameParty game)
    -- Get game step number
    stepN <- readIORef (gameStep game)
    -- Send unit information to all players
    sendToPlayers (encode stepN <> encode (0::Word8)) allUnits players
    -- Increment gamestep
    modifyIORef (gameStep game) (+1)
    -- print allUnits
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