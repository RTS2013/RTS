{-# LANGUAGE Trustworthy #-}

module Start where

import           Blaze.ByteString.Builder
import           Control.Concurrent.STM
import           Data.Binary (Get,get)
import qualified Data.HashTable.IO   as HT
import qualified Data.IntMap         as IM
import qualified Data.Map            as M
import           Data.Monoid ((<>))
import           Data.IORef
import           Data.Sequence ((|>),singleton)
import qualified Grid.Boxed as G
import qualified Grid.Unboxed as GU
import qualified Tree2D as KDT
import           Looping (loopFPS)
import           MIO.MIO
import           Data
import           Competition (Name)
import qualified Competition as Cptn
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import           ToDo (toDoList)
import qualified Pathfinding as P

import Mod.Setup (runMod,defaultGameState,defaultNodeState,defaultTeamState)

type TimeDelta = Double

main :: IO ()
main = do
    namesAndPasses <- fmap read $! readFile "namesAndPasses.txt"
    
    (_,cptn) <- Cptn.begin 4444 (get :: Get ControlMessage) namesAndPasses
    game <- do 
        stepRef   <- newIORef 0
        stateRef  <- newIORef defaultGameState
        teams     <- MV.new $! length namesAndPasses
        behaveRef <- newIORef IM.empty
        kdtRef    <- newIORef $! KDT.empty (radius . unitStaticState) (_v1 . unitMoveState) (_v2 . unitMoveState)
        pathRef   <- newIORef P.empty
        return $! Game 
            { gameStepRef = stepRef
            , gameStateRef = stateRef 
            , gameTeamsVec = teams
            , gameKDTRef   = kdtRef
            , gamePathing  = pathRef
            , gameBehaviorsRef = behaveRef
            }
    -- Add default teams to game
    mapM_ (makeTeam game defaultTeamState) [0..length namesAndPasses - 1]
    -- Run Mod on game
    putStrLn "Applying mod..."
    flip change game $! runMod (length namesAndPasses)
    putStrLn "Modding done."
    -- Start game
    putStrLn "Game is accepting players and streaming info to them."
    loopFPS 10 game (stepGame cptn)

{-# INLINE stepGame #-}
stepGame :: (Cptn.Competition ControlMessage) -> TimeDelta -> Game g u t -> IO (Game g u t)
stepGame cptn td game = do
    -- Apply player commands to units
    Cptn.messages cptn >>= mapM_ (applyControlMessage cptn game)
    gameBehavings <- fmap IM.elems $! readIORef (gameBehaviorsRef game)
    teams <- fmap V.toList $! V.freeze $! gameTeamsVec game
    units <- fmap concat $! sequence $! map (fmap (map snd) . HT.toList . teamUnits) teams
    kdt <- readIORef (gameKDTRef game)
    gamesChanges <- sequence (map (flip behave (game,td)) gameBehavings)
    teamsChanges <- toDoList 1 (map (teamChanges td game) teams)
    unitsChanges <- toDoList 500 (map (unitChanges td game) units)
    -- Apply game changes
    (change $! sequence_ gamesChanges) game
    -- Apply team changes
    (change $! mapM_ sequence_ teamsChanges) game
    -- Apply unit changes
    (change $! mapM_ sequence_ unitsChanges) game
    allUnits <- fmap concat $! sequence $! map (fmap (map snd) . HT.toList . teamUnits) teams
    let teamCount = MV.length $! gameTeamsVec game
    let newKDT = KDT.makeFrom kdt allUnits
    newKDT `seq` writeIORef (gameKDTRef game) newKDT
    stepN <- readIORef (gameStepRef game)
    let header = fromWrite $! writeWord64be (doubleToWord stepN) <> writeWord8 0
    mapM_ (Cptn.sendPiecesToParty cptn 32 header (map buildUnit allUnits)) [0..teamCount-1]
    -- Increment gamestep
    modifyIORef (gameStepRef game) (+1)
    return game

{-# INLINE sendTerrain #-}
sendTerrain :: (Cptn.Competition a) -> Name -> IO ()
sendTerrain cptn name = do
    let header = fromWrite $! writeWord64be (doubleToWord stepN) <> writeWord8 3
    Cptn.sendPiecesPerMicrosecondsToPlayer cptn 10000 1024

{-# INLINE unitChanges #-}
unitChanges :: TimeDelta -> Game g u t -> Unit g u t -> IO [Change (Game g u t) ()]
unitChanges td game u = do
    let behaviors = IM.elems $! unitBehaviors u
    sequence $! map (\b -> behave b (game,td,u)) behaviors

{-# INLINE teamChanges #-}
teamChanges :: TimeDelta -> Game g u t -> Team g u t -> IO [Change (Game g u t) ()]
teamChanges td game t = do
    behaviors <- fmap IM.elems $! readIORef $! teamBehaviors t
    sequence $! map (\b -> behave b (game,td,t)) behaviors

{-# INLINE applyControlMessage #-}
applyControlMessage :: (Cptn.Competition ControlMessage) -> Game g u t -> (ControlMessage,Name) -> IO ()
applyControlMessage cptn game (OrderMsg (Orders shift ids order), player) = do
    playersTeam <- fmap (M.! player) $! atomically $! readTVar $! Cptn.cptnMembers cptn
    team <- MV.read teams playersTeam
    let units = teamUnits team
    flip mapM_ ids $ \unit_id -> do
        maybeUnit <- HT.lookup units unit_id
        case maybeUnit of
            Just unit -> 
                if shift
                then HT.insert units unit_id $! unit {unitOrders = unitOrders unit |> order}
                else HT.insert units unit_id $! unit {unitOrders = singleton order}
            Nothing -> return ()
    where
    teams = gameTeamsVec game
applyControlMessage _ _ (Ready, _) = return ()
applyControlMessage _ _ _ = return ()

{-# INLINE makeTeam #-}
makeTeam :: Game g u t -> t -> Int -> IO ()
makeTeam game teamS i = do
    stateRef <- newIORef teamS
    countRef <- newIORef 0
    behavRef <- newIORef IM.empty
    nilUnits <- HT.newSized 10
    let t = Team
            { teamID = i
            , teamState = stateRef
            , teamSpawnCount = countRef
            , teamUnits = nilUnits
            , teamBehaviors = behavRef
            } 
    MV.write (gameTeamsVec game) i t