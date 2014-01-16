{-
Library for connecting players to teams.
-}

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module GoodTimes 
( GoodTimes()
, Player()
, startGoodTimes
, endGoodTimes
, getMessages
, idlePlayers
, removePlayer
, switchTeam
) where

import           Data.Monoid ((<>))
import           Control.Concurrent.STM
import           Control.Applicative    ((<$>),(<*>))
import           Control.Concurrent     (ThreadId,forkIO,killThread,myThreadId)
import           Control.Monad          (forever)
import           Data.List              (partition)
import           Data.Time.Clock        (UTCTime,diffUTCTime,getCurrentTime)
import           Data.Binary            (Binary,get,put,decodeOrFail)
import           Data.Text.Binary       ()
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import qualified Data.Vector                as V
import qualified Network.WebSockets         as W

type Name = T.Text

type Secret = T.Text

data ClientMessage a = ClientMessage Name Secret a

data Player = Player
    { playerTeam    :: {-# UNPACK #-} !Int
    , playerThread  :: {-# UNPACK #-} !ThreadId
    , playerName    :: !Name
    , playerSecret  :: !Secret
    , playerConn    :: !W.Connection
    , playerLastMsg :: !UTCTime
    }

instance (Binary a) => Binary (ClientMessage a) where
        get = ClientMessage <$> get <*> get <*> get
        put = undefined

data GoodTimes a = GoodTimes
    { serverThread  :: ThreadId
    , playersTVar   :: TVar [Player]
    , messagesTVar  :: TVar [a]
    , teamSlotsTVar :: TVar (V.Vector Int)
    } 

startGoodTimes :: forall a. (Binary a) => [Int] -> IO (GoodTimes a)
startGoodTimes xs = do
        teamSlotsVar  <- newTVarIO $ V.fromList xs
        playersVar    <- newTVarIO []
        messagesVar   <- newTVarIO []
        serversThread <- forkIO $ W.runServer "0.0.0.0" 9160 $ tryConnect teamSlotsVar playersVar messagesVar
        return $ GoodTimes serversThread playersVar messagesVar teamSlotsVar
    where 
        tryConnect :: TVar (V.Vector Int) -> TVar [Player] -> TVar [a] -> W.PendingConnection -> IO ()
        tryConnect teamSlotsVar playersVar messagesVar pendingConn = do
            conn <- W.acceptRequest pendingConn
            msg <- W.receiveDataMessage conn
            case msg of
                W.Text _ -> TIO.putStrLn "Somebodies hello message was in a textual format."
                W.Binary binData -> case decodeOrFail binData of
                    Left _ -> TIO.putStrLn "Somebodies hello message wasn't encoded properly."
                    Right (_,_,ClientMessage name secret (_ :: a)) -> do
                        myThread <- myThreadId
                        myTime <- getCurrentTime
                        -- Attempt to join team
                        joinedTeam <- atomically $ do
                            let writeInPlayer :: (Int, V.Vector Int) -> STM Player
                                writeInPlayer (team,newSlots) = do
                                    writeTVar teamSlotsVar newSlots
                                    let player = Player team myThread name secret conn myTime
                                    players <- readTVar playersVar
                                    writeTVar playersVar $ player:players
                                    return player
                            -- Attempt to add player to first available team
                            attemptedJoin <- getFirstSlot `fmap` readTVar teamSlotsVar
                            maybe (return Nothing) (\aj -> writeInPlayer aj >>= return . Just) attemptedJoin
                        case joinedTeam of
                            Nothing -> TIO.putStrLn $ name <> " failed to join a team."
                            Just player -> do
                                TIO.putStrLn $ playerName player <> " has joined team " <> (T.pack . show) (playerTeam player) <> "."
                                acceptMessages player messagesVar
        acceptMessages :: Player -> TVar [a] -> IO ()
        acceptMessages player messagesVar = forever $ do
            datagram <- W.receiveDataMessage (playerConn player)
            case datagram of
                W.Text _ -> TIO.putStrLn "Somebodies data was in a textual format."
                W.Binary binData -> case decodeOrFail binData of
                    Left _ -> TIO.putStrLn "Somebodies data wasn't encoded properly."
                    Right (_,_,ClientMessage name secret (msg :: a)) -> do
                        if name == playerName player && secret == playerSecret player
                        then atomically $ readTVar messagesVar >>= writeTVar messagesVar . (msg:)
                        else TIO.putStrLn "Somebodies name or secret was wrong."
        getFirstSlot :: V.Vector Int -> Maybe (Int, V.Vector Int)
        getFirstSlot v = subF 0 
            where
            subF i = flip (maybe Nothing) (v V.!? i) $ 
                        \n -> if n > 0 
                              then Just (i,v V.// [(i,n - 1)]) 
                              else subF (i + 1)

endGoodTimes :: GoodTimes a -> IO ()
endGoodTimes = killThread . serverThread


getMessages :: GoodTimes a -> IO [a]
getMessages gt = atomically $ do
    let msgsVar = messagesTVar gt
    msgs <- readTVar msgsVar
    writeTVar msgsVar []
    return msgs

idlePlayers :: Rational -> GoodTimes a -> IO [Player]
idlePlayers duration gt = do
    let playersVar = playersTVar gt
    timeNow <- getCurrentTime
    atomically $ readTVar playersVar >>= 
        return . filter (\player -> toRational (diffUTCTime timeNow $ playerLastMsg player) > duration)

removePlayer :: GoodTimes a -> Player -> IO ()
removePlayer gt player = do
    badPlayers <- atomically $ do
        let playersVar = playersTVar gt
        players <- readTVar playersVar
        let (badPlayers,goodPlayers) = partition ((==(playerName player)) . playerName) players
        writeTVar playersVar goodPlayers
        let teamSlotsVar = teamSlotsTVar gt
        teamSlots <- readTVar teamSlotsVar
        let teamSlots' = foldl 
                (\ts badPlayer -> 
                    ts V.// [(playerTeam badPlayer, ts V.! (playerTeam badPlayer) - 1)]
                ) teamSlots badPlayers
        writeTVar teamSlotsVar teamSlots'
        return badPlayers
    mapM_ (killThread . playerThread) badPlayers

switchTeam :: GoodTimes a -> Int -> Player -> IO Bool
switchTeam gt team player = atomically $ do
    let teamSlotsVar = teamSlotsTVar gt
    teamSlots <- readTVar teamSlotsVar
    case teamSlots V.!? team of
        Nothing -> return False
        Just ok -> if ok <= 0 then return False else do
            let currentTeam = playerTeam player
            writeTVar teamSlotsVar $ teamSlots V.// [(team, teamSlots V.! team - 1)
                                                    ,(currentTeam, teamSlots V.! currentTeam + 1)]
            let playersVar = playersTVar gt
            players <- readTVar playersVar
            writeTVar playersVar $ map  
                (\p -> if playerName p == playerName player
                       then player {playerTeam=team} 
                       else p
                ) players
            return True