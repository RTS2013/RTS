{-# LANGUAGE Trustworthy #-}

module Start where

import qualified Data.HashTable.IO as HT
import qualified Data.IntMap       as IM
import qualified Grid.UnboxedGrid  as Grid
import qualified KDT               as KDT
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
    putStrLn "Enter a list of teams (Ex. [4,1,1,2] )"
    teamNums <- fmap read getLine
    putStrLn "Game is accepting players and streaming info to them."
    party    <- openDoors 4444 teamNums :: IO (Party ControlMessage)
    defaultGame <- do 
        stepRef  <- newIORef 0
        stateRef <- newIORef defaultGameState
        teams <- HT.new
        grid  <- Grid.make (1024,1024) defaultTileState
        valuesRef <- newIORef $ \_ -> []
        behaveRef <- newIORef IM.empty
        kdtRef    <- newIORef KDT.empty
        return $ Game 
            { gameStep = stepRef
            , gameState = stateRef 
            , gameParty = party
            , gameTeams = teams
            , gameKDT   = kdtRef
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

loop :: Int64 -> (a -> IO ()) -> a -> IO ()
loop fps f game = getCurrentTime >>= actualLoop 1
    where
    actualLoop steps time = do
        f game
        timeNow <- getCurrentTime
        threadDelay $ max 0 $ fromIntegral $ 
            (steps * 1000000 `div` fps) - 
            ceiling (diffUTCTime timeNow time * fromInteger 1000000)
        actualLoop (steps + 1) time 

stepGame :: Game gameS teamS unitS tileS -> IO ()
stepGame game = do
    -- Apply player commands to units
    getMessages (gameParty game) >>= mapM_ (applyControlMessage game)
    gameBehavings <- fmap IM.elems $ readIORef (gameBehaviors game)
    teams <- fmap (map snd) $ HT.toList $ gameTeams game
    units <- fmap concat $ sequence $ map (fmap (map snd) . HT.toList . teamUnits) teams
    let kdt = KDT.makePar [positionX . unitMoveState, positionY . unitMoveState] units
    writeIORef (gameKDT game) kdt
    gamesChanges <- parallel (map (toIO . ($game)) gameBehavings)
    teamsChanges <- parallel (map teamChanges teams)
    unitsChanges <- parallel (map unitChanges units)
    -- Apply game changes
    toIO $ sequence_ gamesChanges
    -- Apply team changes
    toIO $ mapM_ sequence_ teamsChanges
    -- Apply unit changes
    toIO $ mapM_ sequence_ unitsChanges
    allUnits <- fmap concat $ sequence $ map (fmap (map snd) . HT.toList . teamUnits) teams
    players <- allPlayers (gameParty game)
    stepN <- readIORef (gameStep game)
    sendToPlayers (encode stepN <> encode (0::Word8)) allUnits players
    -- Increment gamestep
    modifyIORef (gameStep game) (+1)

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