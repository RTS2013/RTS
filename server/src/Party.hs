{- Library for connecting players to teams. -}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Party
( Party()
, Player(playerTeam,playerName)
, openDoors
, closeDoors
, getMessages
, allPlayers
, idlePlayers
, removePlayer
, switchTeam
, sendToPlayers
) where 

import           Control.Exception      (onException)
import           Control.Applicative    ((<*>),(<$>))
import           Control.Concurrent.STM 
import           Control.Concurrent     (ThreadId,forkIO,killThread,myThreadId)
import           Control.Monad          (forever,filterM)
import           Blaze.ByteString.Builder
import           Data.Int               (Int64)
import           Data.List              (partition)
import           Data.Monoid            ((<>))
import           Data.Time.Clock        (UTCTime,diffUTCTime,getCurrentTime)
import           Data.Binary            (Binary,Get,get,put,decodeOrFail,encode)
import           Data.Binary.Get        (getByteString)
import           Data.Binary.Put        (putByteString)
import           Data.Word              (Word16,Word32)
import           Data.Text.Encoding     (decodeUtf8,encodeUtf8)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified Data.Vector          as V
import qualified Network.WebSockets   as W
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString      as SB

type Name = T.Text

type Secret = T.Text

type TeamID = Int

type Slots = V.Vector Int

data ClientMessage a = ClientMessage Name Secret a

data Player = Player
    { playerTeam    :: {-# UNPACK #-} !TeamID
    , playerThread  :: {-# UNPACK #-} !ThreadId
    , playerName    :: !Name
    , playerSecret  :: !Secret
    , playerConn    :: !W.Connection
    , playerLastMsg :: !(TVar UTCTime)
    }

instance (Binary a) => Binary (ClientMessage a) where
    put = undefined
    get = ClientMessage <$> get <*> get <*> get

instance Binary T.Text where
    put a = let bs = encodeUtf8 a in (put $ (fromIntegral $ SB.length bs :: Word32)) >> putByteString bs
    get = (get :: Get Word32) >>= getByteString . fromIntegral >>= return . decodeUtf8

data Party a = Party
    { serverThread  :: ThreadId
    , playersTVar   :: TVar [Player]
    , messagesTVar  :: TVar [(Player,a)]
    , teamSlotsTVar :: TVar Slots
    } 

openDoors :: forall a. (Binary a) => Int -> [Int] -> IO (Party a)
openDoors port xs = do
        teamSlotsVar  <- newTVarIO $ V.fromList xs
        playersVar    <- newTVarIO []
        messagesVar   <- newTVarIO []
        serversThread <- forkIO $ W.runServer "0.0.0.0" port $ tryConnect teamSlotsVar playersVar messagesVar
        return $ Party serversThread playersVar messagesVar teamSlotsVar
    where 
        tryConnect :: TVar Slots -> TVar [Player] -> TVar [(Player,a)] -> W.PendingConnection -> IO ()
        tryConnect teamSlotsVar playersVar messagesVar pendingConn = do
            putStrLn "Attempted connection."
            conn <- W.acceptRequest pendingConn
            msg <- W.receiveDataMessage conn
            case msg of
                W.Text _ -> TIO.putStrLn "Somebodies hello message was in a textual format."
                W.Binary binData -> case decodeOrFail binData of
                    Left _ -> TIO.putStrLn "Somebodies hello message wasn't encoded properly."
                    Right (_,_,ClientMessage name secret (_ :: ())) -> do
                        myThread <- myThreadId
                        myTime <- getCurrentTime
                        -- Attempt to join team
                        joinedTeam <- atomically $ do
                            let writeInPlayer :: (TeamID, Slots) -> STM Player
                                writeInPlayer (team,newSlots) = do
                                    writeTVar teamSlotsVar newSlots
                                    timeTVar <- newTVar myTime
                                    let player = Player team myThread name secret conn timeTVar
                                    players <- readTVar playersVar
                                    writeTVar playersVar $ player:players
                                    return player
                            -- Attempt to add player to first available team
                            attemptedJoin <- getFirstSlot `fmap` readTVar teamSlotsVar
                            maybe (return Nothing) (fmap Just . writeInPlayer) attemptedJoin
                        case joinedTeam of
                            Nothing -> TIO.putStrLn $ name <> " failed to join a team."
                            Just player -> do
                                TIO.putStrLn $ playerName player 
                                            <> " has joined team " 
                                            <> (T.pack . show) (playerTeam player) 
                                            <> " with secret: "
                                            <> playerSecret player
                                acceptMessages player messagesVar

        acceptMessages :: Player -> TVar [(Player,a)] -> IO ()
        acceptMessages player messagesVar = forever $ do
            datagram <- W.receiveDataMessage (playerConn player)
            case datagram of
                W.Text _ -> TIO.putStrLn "Somebodies data was in a textual format."
                W.Binary binData -> case decodeOrFail binData of
                    Left _ -> TIO.putStrLn "Somebodies data wasn't encoded properly."
                    Right (_,_,ClientMessage name secret (msg :: a)) -> do
                        if name == playerName player && secret == playerSecret player
                        then do
                            timeNow <- getCurrentTime
                            atomically $ do
                                readTVar messagesVar >>= writeTVar messagesVar . ((player, msg):)
                                writeTVar (playerLastMsg player) timeNow
                            TIO.putStrLn $ "Received message from " <> playerName player
                        else TIO.putStrLn "Somebodies name or secret was wrong."

        getFirstSlot :: Slots -> Maybe (TeamID, Slots)
        getFirstSlot v = subF 0 
            where
            subF i = flip (maybe Nothing) (v V.!? i) $ 
                        \n -> if n > 0 
                              then Just (i,v V.// [(i,n - 1)]) 
                              else subF (i + 1)

closeDoors :: Party a -> IO ()
closeDoors = killThread . serverThread

getMessages :: Party a -> IO [(Player,a)]
getMessages gt = atomically $ do
    let msgsVar = messagesTVar gt
    msgs <- readTVar msgsVar
    writeTVar msgsVar []
    return msgs

allPlayers :: Party a -> IO [Player]
allPlayers = atomically . readTVar . playersTVar

idlePlayers :: Rational -> Party a -> IO [Player]
idlePlayers duration gt = do
    let playersVar = playersTVar gt
    timeNow <- getCurrentTime
    atomically $ readTVar playersVar >>= 
        filterM (\player -> do
            lastTime <- readTVar $ playerLastMsg player
            return $ toRational (diffUTCTime timeNow $ lastTime) > duration
        )

removePlayer :: Party a -> Player -> IO ()
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

switchTeam :: Party a -> Player -> TeamID -> IO Bool
switchTeam gt player team = atomically $ do
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
                (\p -> if playerName p == playerName player && playerSecret p == playerSecret player
                       then player {playerTeam=team} 
                       else p
                ) players
            return True


sendToPlayers :: (Binary a) => BS.ByteString -> [a] -> [Player] -> IO ()
sendToPlayers header list players = do
    _ <- forkIO $ do
        let choppy = chopList header $ map encode list
        mapM_ (\p -> mapM_ (sendMsg (playerConn p)) choppy) players
    return ()
    where
    sendMsg :: W.Connection -> BS.ByteString -> IO ()
    sendMsg conn = flip onException (putStrLn "You suck")
                 . W.send conn . W.DataMessage . W.Binary

{-
sendToPlayers :: (Binary a) => BS.ByteString -> [a] -> [Player] -> IO ()
sendToPlayers header list players = do
    _ <- forkIO $ do
        let choppy = choppy2 header $ map encode list
        mapM_ (\p -> sendMsg (playerConn p) choppy) players
    return ()
    where
    sendMsg :: W.Connection -> BS.ByteString -> IO ()
    sendMsg conn = flip onException (putStrLn "You suck")
                 . W.send conn . W.DataMessage . W.Binary

choppy2 :: BS.ByteString -> [BS.ByteString] -> BS.ByteString
choppy2 header pieces = tlbs $ foldl (<>) (hBld <> fromWord16be (fromIntegral (length pieces) :: Word16)) $ map flbs pieces
    where
    tlbs = toLazyByteString
    flbs = fromLazyByteString
    hBld = flbs header
-}

chopList :: BS.ByteString -> [BS.ByteString] -> [BS.ByteString]
chopList header pieces = chopper 0 (hLen + 2) (flbs BS.empty) pieces []
    where
    tlbs = toLazyByteString
    flbs = fromLazyByteString
    hLen = BS.length header
    hBld = flbs header
    packetSize = 2048
    chopper :: Int64 -> Int64 -> Builder -> [BS.ByteString] -> [BS.ByteString] -> [BS.ByteString]
    chopper n accLen acc (x:xs) bs =
        let xLen = BS.length x
            currentSize = xLen + accLen in
        if currentSize < packetSize 
        -- Add to chunk
        then chopper (n + 1) currentSize (acc <> flbs x) xs bs
        -- Start new chunk
        else chopper 1 (hLen + xLen + 2) (flbs x) xs (tlbs (hBld <> fromWord16be (fromIntegral n :: Word16) <> acc) : bs)
    chopper n _ acc _ bs = (tlbs (hBld <> fromWord16be (fromIntegral n :: Word16) <> acc) : bs)