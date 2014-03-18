{-# LANGUAGE Trustworthy #-}

module Start where

import qualified Data.HashTable.IO   as HT
import qualified Data.IntMap         as IM
import qualified Grid.Unboxed        as G
import qualified KDT                 as KDT
import Data.Monoid ((<>))
import Data.Binary (encode)
import Data.IORef
import Control.Concurrent.ParallelIO.Global (parallel,stopGlobalPool)
import Data.Int (Int64)
import Data.Sequence ((|>),singleton)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (diffUTCTime,getCurrentTime)
import Data.Word (Word8)
import MIO.MIO
import Mod.Prelude (makeTeam)
import Data
import Party
import safe Mod.Setup (runMod,defaultGameState,defaultTileState,defaultTeamState)

main :: IO ()
main = do
    putStrLn "Enter a list of teams (Ex. [4,1,1,2] )"
    teamNums <- fmap read getLine
    putStrLn "Game is accepting players and streaming info to them."
    party    <- openDoors 4444 teamNums :: IO (Party ControlMessage)
    game <- do 
        stepRef  <- newIORef 0
        stateRef <- newIORef defaultGameState
        teams <- HT.new
        behaveRef <- newIORef IM.empty
        kdtRef    <- newIORef KDT.empty
        return $ Game 
            { gameStep = stepRef
            , gameState = stateRef 
            , gameParty = party
            , gameTeams = teams
            , gameKDT   = kdtRef
            , gameTiles = G.make (0,0) defaultTileState
            , gameBehaviors = behaveRef
            }
    -- Add default teams to game
    flip train game $ mapM_ (makeTeam defaultTeamState) [0..length teamNums - 1]
    -- Run Mod on game
    flip train game $ runMod (length teamNums)
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
            ceiling (diffUTCTime timeNow time * 1000000)
        actualLoop (steps + 1) time 

stepGame :: Game gameS teamS unitS tileS -> IO ()
stepGame game = do
    -- Apply player commands to units
    getMessages (gameParty game) >>= mapM_ (applyControlMessage game)
    gameBehavings <- fmap IM.elems $ readIORef (gameBehaviors game)
    teams <- fmap (map snd) $ HT.toList $ gameTeams game
    units <- fmap concat $ sequence $ map (fmap (map snd) . HT.toList . teamUnits) teams
    let kdt = {-# SCC "makeKDT" #-} KDT.make (radius . unitMoveStats) [msX . unitMoveState, msY . unitMoveState] units
    writeIORef (gameKDT game) kdt
    gamesChanges <- sequence (map (flip behave ()) gameBehavings)
    teamsChanges <- sequence (map teamChanges teams)
    unitsChanges <- sequence (map unitChanges units)
    -- Apply game changes
    change $ sequence_ gamesChanges
    -- Apply team changes
    change $ mapM_ sequence_ teamsChanges
    -- Apply unit changes
    change $ mapM_ sequence_ unitsChanges
    allUnits <- fmap concat $ sequence $ map (fmap (map snd) . HT.toList . teamUnits) teams
    players <- allPlayers (gameParty game)
    stepN <- readIORef (gameStep game)
    sendToPlayers ((encode $ doubleToWord stepN) <> encode (0::Word8)) allUnits players
    -- Increment gamestep
    modifyIORef (gameStep game) (+1)

unitChanges :: Unit gameS teamS unitS tileS -> IO [Change ()]
unitChanges u = do
    let behaviors = IM.elems $ unitBehaviors u
    sequence $ map (\b -> behave b u) behaviors

teamChanges :: Team gameS teamS unitS tileS -> IO [Change ()]
teamChanges t = do
    let behaviors = IM.elems $ teamBehaviors t
    sequence $ map (\b -> behave b t) behaviors

applyControlMessage :: Game gameS teamS unitS tileS -> (Player, ControlMessage) -> IO ()
applyControlMessage game (player, OrderMsg (Orders shift ids order)) = do
    maybeTeam <- HT.lookup teams playersTeam
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
    playersTeam = playerTeam player
applyControlMessage _ _ = return ()