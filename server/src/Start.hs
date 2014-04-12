{-# LANGUAGE Trustworthy #-}

module Start where

import           Blaze.ByteString.Builder
import           Control.Concurrent.STM
import qualified Data.HashTable.IO   as HT
import qualified Data.IntMap         as IM
import qualified Grid.UnboxedMutable as G
import qualified KDTree              as KDT
import qualified Data.Map            as M
import           Data.Monoid ((<>))
import           Data.Binary (Get,get)
import           Data.IORef
import           Control.Concurrent.ParallelIO.Global (parallel,stopGlobalPool)
import           Data.Sequence ((|>),singleton,empty)
import           Looping (loopImpure)
import           MIO.MIO
import           Data
import           Competition (Name)
import qualified Competition as Cptn
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import safe Mod.Setup (runMod,defaultGameState,defaultTileState,defaultTeamState)

main :: IO ()
main = do
    namesAndPasses <- fmap read $ readFile "namesAndPasses.txt"
    putStrLn "Game is accepting players and streaming info to them."
    (_,cptn) <- Cptn.begin 4444 (get :: Get ControlMessage) namesAndPasses
    game <- do 
        stepRef   <- newIORef 0
        stateRef  <- newIORef defaultGameState
        teams     <- MV.new $ length namesAndPasses
        behaveRef <- newIORef IM.empty
        kdtRef    <- newIORef $ KDT.empty (radius . unitMoveStats) [msX . unitMoveState, msY . unitMoveState]
        tileGrid  <- G.make (0,0) defaultTileState
        return $ Game 
            { gameStep = stepRef
            , gameState = stateRef 
            , gameParty = cptn
            , gameTeams = teams
            , gameKDT   = kdtRef
            , gameTiles = tileGrid
            , gameBehaviors = behaveRef
            }
    -- Add default teams to game
    mapM_ (makeTeam game defaultTeamState) [0..length namesAndPasses - 1]
    -- Run Mod on game
    flip train game $ runMod (length namesAndPasses)
    -- Start game
    loopImpure 10 stepGame game
    stopGlobalPool

stepGame :: Game gameS teamS unitS tileS -> IO ()
stepGame game = do
    -- Apply player commands to units
    Cptn.getMessages (gameParty game) >>= mapM_ (applyControlMessage game)
    gameBehavings <- fmap IM.elems $ readIORef (gameBehaviors game)
    teams <- fmap V.toList $ V.freeze $ gameTeams game
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
    let teamCount = MV.length $ gameTeams game
    stepN <- readIORef (gameStep game)
    --sendToPlayers ((encode $ doubleToWord stepN) <> encode (0::Word8)) allUnits playerCount
    mapM_ (Cptn.sendPiecesToParty (gameParty game) 32 (fromWrite $ writeWord64be (doubleToWord stepN) <> writeWord8 0) (map buildUnit allUnits)) [0..teamCount-1]
    -- Increment gamestep
    modifyIORef (gameStep game) (+1)

unitChanges :: Unit gameS teamS unitS tileS -> IO [Change ()]
unitChanges u = do
    let behaviors = IM.elems $ unitBehaviors u
    sequence $ map (\b -> behave b u) behaviors

teamChanges :: Team gameS teamS unitS tileS -> IO [Change ()]
teamChanges t = do
    behaviors <- fmap IM.elems $ readIORef $ teamBehaviors t
    sequence $ map (\b -> behave b t) behaviors

applyControlMessage :: Game gameS teamS unitS tileS -> (ControlMessage,Name) -> IO ()
applyControlMessage game (OrderMsg (Orders shift ids order),player) = do
    playersTeam <- fmap (M.! player) $ atomically $ readTVar . Cptn.cptnMembers $ gameParty game 
    team <- MV.read teams playersTeam
    let units = teamUnits team
    flip mapM_ ids $ \unit_id -> do
        maybeUnit <- HT.lookup units unit_id
        case maybeUnit of
            Just unit -> 
                if shift
                then HT.insert units unit_id $ unit {unitOrders = unitOrders unit |> order}
                else HT.insert units unit_id $ unit {unitOrders = singleton order}
            Nothing -> return ()
    where
    teams = gameTeams game
applyControlMessage _ _ = return ()

makeTeam :: Game gameS teamS unitS tileS -> teamS -> Int -> IO ()
makeTeam game teamS i = do
    stateRef <- newIORef teamS
    countRef <- newIORef 0
    behavRef <- newIORef IM.empty
    grid     <- G.make (0,0) (-1)
    noUnits  <- HT.newSized 10
    discovered <- newIORef empty
    sendTilesVar <- newTVarIO []
    let t = Team
            { teamID = i
            , teamState = stateRef
            , teamVision = grid
            , teamSpawnCount = countRef
            , teamUnits = noUnits
            , teamBehaviors = behavRef
            , teamDiscovered = discovered
            , teamSendTiles  = sendTilesVar
            } 
    MV.write (gameTeams game) i t