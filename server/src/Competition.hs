{-# LANGUAGE OverloadedStrings #-}

module Competition 
( messages
, joiners
, playerTeam
, begin
, sendPiecesToParty
, sendPiecesPerMicrosecondsToPlayer
, Competition(..)
, Name
) where

import           Blaze.ByteString.Builder
import           Control.Concurrent.STM
import           Control.Concurrent ( ThreadId
                                    , forkIO
                                    , killThread
                                    , myThreadId
                                    , threadDelay )
import           Control.Exception (onException)
import           Control.Monad (forever)
import           Data.Binary (get)
import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as G
import           Data.ByteString.Lazy (ByteString)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.List (genericLength)
import           Data.List.Split (chunksOf)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid (mconcat,(<>))
import qualified Data.Text.IO as TIO
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Clock
import           Data.Word (Word8)
import           Network.WebSockets (Connection)
import qualified Network.WebSockets as W

type PartyID = Int
type Name = Text
type Pass = Text

----------------------------------------------------------
-- | A competition consisting of parties and party members
data Competition a = Competition
    { cptnParties :: TVar (IntMap (Map Name Member))
    , cptnMembers :: TVar (Map Name PartyID)
    , cptnMailbox :: TVar [(a, Name)]
    , cptnJoiners :: TVar [Name]
    } 

data Member = Member
    { memberPass   :: TVar Pass
    , memberLast   :: TVar UTCTime
    , memberStatus :: TVar Status
    } 

data Status = Connected Connection ThreadId
            | Disconnected

messages :: Competition a -> IO [(a,Name)]
messages cptn = atomically $ do
    let mailbox = cptnMailbox cptn
    msgs <- readTVar mailbox
    writeTVar mailbox []
    return msgs

playerTeam :: Competition a -> Name -> IO Int
playerTeam cptn name = fmap (M.! name) $ atomically $ readTVar (cptnMembers cptn)

-- List of players who joined since the last time this function was called
joiners :: Competition a -> IO [Name]
joiners cptn = atomically $ do
    joins <- readTVar (cptnJoiners cptn)
    writeTVar (cptnJoiners cptn) []
    return joins

begin :: Int -> Get a -> [[(Name,Pass)]] -> IO (ThreadId,Competition a)
begin port decodeMsg namesAndPasses = do
    beginTime    <- Clock.getCurrentTime
    partyMembers <- mapM (mapM (newMember beginTime)) namesAndPasses
    partiesVar   <- newTVarIO $ IM.fromList $ zip [0..] $ map M.fromList partyMembers
    membersVar   <- newTVarIO $ M.fromList $ concat $ zipWith (\i xs -> map (\v -> (fst v,i)) xs) [0..] $ namesAndPasses
    mailboxVar   <- newTVarIO []
    joinersVar   <- newTVarIO []
    let cptn = Competition { cptnParties = partiesVar
                           , cptnMailbox = mailboxVar
                           , cptnMembers = membersVar 
                           , cptnJoiners = joinersVar }
    cptnThread <- forkIO $ W.runServer "0.0.0.0" port $ \pconn -> do
        conn  <- W.acceptRequest pconn
        hello <- W.receive conn
        case hello of
            W.DataMessage (W.Binary bs) -> 
                case G.runGetOrFail decodeHello bs of
                    Left _ -> W.sendClose conn ("Bad hello message." :: ByteString)
                    Right (_,_,(name,pass)) -> do
                        joinThread <- myThreadId
                        joinTime   <- Clock.getCurrentTime
                        joined <- atomically $ do
                            members <- readTVar membersVar
                            parties <- readTVar partiesVar
                            if M.member name members then 
                                let pID = members M.! name
                                    party = parties IM.! pID
                                    member = party M.! name in do
                                    pwd <- readTVar $ memberPass member
                                    if pwd /= pass
                                    then return Nothing
                                    else do
                                        writeTVar (memberLast member) joinTime
                                        writeTVar (memberStatus member) $ Connected conn joinThread
                                        modifyTVar' joinersVar (name:)
                                        return $ Just member
                            else 
                                return Nothing
                        case joined of
                            Just member -> do
                                TIO.putStrLn (name <> " connected.")
                                forever $ do
                                    msg <- onException (W.receive conn) $ do
                                        TIO.putStrLn $ name <> " disconnected."
                                        atomically $ writeTVar (memberStatus member) Disconnected
                                        myThreadId >>= killThread
                                    case msg of 
                                        W.DataMessage (W.Binary msgBS) -> 
                                            case G.runGetOrFail decodeMsg msgBS of
                                                Left _ -> do
                                                    TIO.putStrLn $ name <> " sent an incorrectly encoded message."
                                                    atomically $ writeTVar (memberStatus member) Disconnected
                                                    W.sendClose conn ("Bad message encoding." :: ByteString)
                                                    myThreadId >>= killThread
                                                Right (_,_,newMsg) -> do
                                                    lastTime <- Clock.getCurrentTime
                                                    atomically $ do
                                                        writeTVar (memberLast member) lastTime
                                                        mail <- readTVar mailboxVar 
                                                        writeTVar mailboxVar $ (newMsg,name):mail
                                        _ -> do
                                            TIO.putStrLn $ name <> " used the wrong message format."
                                            atomically $ writeTVar (memberStatus member) Disconnected
                                            W.sendClose conn ("Bad message format." :: ByteString)
                                            myThreadId >>= killThread
                            Nothing -> do 
                                TIO.putStrLn $ name <> " failed to join."
                                W.sendClose conn ("Failed to join." :: ByteString)
                                myThreadId >>= killThread
            _ -> W.sendClose conn ("Bad message format." :: ByteString)
    return (cptnThread,cptn)

newMember :: UTCTime -> (Name,Pass) -> IO (Name,Member)
newMember time (name,pass) = do
    passVar <- newTVarIO pass
    timeVar <- newTVarIO time
    statVar <- newTVarIO Disconnected
    return (name
           , Member { memberPass   = passVar
                    , memberLast   = timeVar
                    , memberStatus = statVar })


sendPiecesToParty :: Competition a -> Int -> Builder -> [Builder] -> Int -> IO ()
sendPiecesToParty cptn chunkSize header pieces pID = ignore $ forkIO $ do
    members <- atomically $ fmap (IM.! pID) $ readTVar (cptnParties cptn) 
    let choppedList = chopList chunkSize header pieces
    flip mapM_ (M.elems members) $ withMemberConn (\c -> mapM_ (sendMsg c) choppedList)
    where
    ignore m = m >> return ()


chopList :: Int -> Builder -> [Builder] -> [ByteString]
chopList chunkSize header pieces
    = map (\xs -> toLazyByteString . mconcat 
    $ (header:fromWord16be (genericLength xs):xs)) 
    $ chunksOf chunkSize pieces


sendMsg :: Connection -> ByteString -> IO ()
sendMsg conn = flip onException (TIO.putStrLn "You suck")
             . W.send conn . W.DataMessage . W.Binary


decodeText :: Get Text
decodeText = (get :: Get Word8) >>= 
             (G.getByteString . fromIntegral) >>= 
             (return . decodeUtf8)


decodeHello :: Get (Name,Pass)
decodeHello = do
    text <- decodeText
    pass <- decodeText
    return (text,pass)


withMemberConn :: (Connection -> IO ()) -> Member -> IO ()
withMemberConn f m = do
    ms <- atomically $ readTVar $ memberStatus m
    case ms of
        Connected conn _ -> f conn
        _                -> return ()

sendPiecesPerMicrosecondsToPlayer :: Competition a -> Int -> Int -> Builder -> [Builder] -> Name -> IO ()
sendPiecesPerMicrosecondsToPlayer cptn microseconds chunkSize header pieces name = ignore $ forkIO $ do
    pID <- playerTeam cptn name
    members <- atomically $ fmap (IM.! pID) $ readTVar (cptnParties cptn) 
    let choppedList = chopList chunkSize header pieces
    withMemberConn (\c -> mapM_ (\bs -> sendMsg c bs >> threadDelay microseconds) choppedList) $ members M.! name
    where
    ignore m = m >> return ()