{-# LANGUAGE OverloadedStrings #-}

module Kunnect where

import           Blaze.ByteString.Builder
import           Control.Concurrent.STM
import           Control.Concurrent ( ThreadId
                                    , forkIO
                                    , killThread
                                    , myThreadId )
import           Control.Exception (onException)
import           Control.Monad (forever)
import           Data.Binary (get)
import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as G
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Int (Int64)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.Text.IO as TIO
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Clock
import           Data.Word ( Word8
                           , Word16 )
import           Network.WebSockets (Connection)
import qualified Network.WebSockets as W

type PartyID = Int
type Name = Text
type Pass = Text

----------------------------------------------------------
-- | A competition consisting of parties and party members
data Competition a = Competition
    { cptnParties :: TVar (IntMap Party)
    , cptnMailbox :: TVar [(a, Name)]
    , cptnMembers :: TVar (Map Name PartyID)
    } 


type Party = Map Name Member


data Member = Member
    { memberPass   :: TVar Pass
    , memberLast   :: TVar UTCTime
    , memberStatus :: TVar Status
    } 


data Status = Connected Connection ThreadId
            | Disconnected


decodeText :: Get Text
decodeText = (get :: Get Word8) >>= (G.getByteString . fromIntegral) >>= (return . decodeUtf8)


decodeHello :: Get (Name,Pass)
decodeHello = do
    text <- decodeText
    pass <- decodeText
    return (text,pass)


disconnected :: Member -> IO Bool
disconnected m = do
    ms <- atomically $ readTVar $ memberStatus m
    return $ case ms of
        Disconnected -> True
        _            -> False


withMemberConn :: (Connection -> IO ()) -> Member -> IO ()
withMemberConn f m = do
    ms <- atomically $ readTVar $ memberStatus m
    case ms of
        Connected conn _ -> f conn
        _                -> return ()


begin :: Int -> Get a -> [[(Name,Pass)]] -> IO (ThreadId,Competition a)
begin port decodeMsg namesAndPasses = do
    timeNow <- Clock.getCurrentTime
    partyMembers <- mapM (mapM (newMember timeNow)) namesAndPasses
    partiesVar <- newTVarIO $ IM.fromList $ zip [0..] $ map M.fromList partyMembers
    membersVar <- newTVarIO $ M.fromList $ concat $ zipWith (\i xs -> map (\v -> (fst v,i)) xs) [0..] $ namesAndPasses
    mailboxVar <- newTVarIO []
    let cptn = Competition { cptnParties = partiesVar
                           , cptnMailbox = mailboxVar
                           , cptnMembers = membersVar }
    cptnThread <- forkIO $ W.runServer "0.0.0.0" port $ \pconn -> do
        conn  <- W.acceptRequest pconn
        hello <- W.receive conn
        case hello of
            W.DataMessage (W.Binary bs) -> 
                case G.runGetOrFail decodeHello bs of
                    Left _ -> W.sendClose conn ("Bad message encoding." :: ByteString)
                    Right (_,_,(name,pass)) -> ignore $ forkIO $ do
                        memberThread <- myThreadId
                        memberTime   <- Clock.getCurrentTime >>= newTVarIO
                        joined <- atomically $ do
                            members <- readTVar membersVar
                            parties <- readTVar partiesVar
                            if M.member name members then 
                                undefined
                            else 
                                return False
                        if joined
                        then forever $ do
                            msg <- onException (W.receive conn) $ do
                                TIO.putStrLn $ name <> " disconnected."
                                myThreadId >>= killThread
                            case msg of 
                                W.DataMessage (W.Binary msgBS) -> 
                                    case G.runGetOrFail decodeMsg msgBS of
                                        Left _ -> do
                                            W.sendClose conn ("Bad message encoding." :: ByteString)
                                            myThreadId >>= killThread
                                        Right (_,_,newMsg) -> do
                                            lastTime <- Clock.getCurrentTime
                                            atomically $ do
                                                writeTVar memberTime lastTime
                                                mail <- readTVar mailboxVar 
                                                writeTVar mailboxVar $ (newMsg,name):mail
                                _ -> do
                                    W.sendClose conn ("Bad message format." :: ByteString)
                                    myThreadId >>= killThread
                        else do
                            W.sendClose conn ("Failed to join." :: ByteString)
                            myThreadId >>= killThread
            _ -> W.sendClose conn ("Bad message format." :: ByteString)
    return (cptnThread,cptn)
    where
    ignore m = m >> return ()


newMember :: UTCTime -> (Name,Pass) -> IO (Name,Member)
newMember time (name,pass) = do
    passVar <- newTVarIO pass
    timeVar <- newTVarIO time
    statVar <- newTVarIO Disconnected
    return (name
           , Member { memberPass   = passVar
                    , memberLast   = timeVar
                    , memberStatus = statVar })


sendPiecesToParty :: Competition a -> Int -> ByteString -> [ByteString] -> IO ()
sendPiecesToParty cptn pID header pieces = do
    _ <- forkIO $ do
        let choppy = {-# SCC "choppy" #-} chopList header pieces
        members <- atomically $ fmap (IM.! pID) $ readTVar (cptnParties cptn) 
        flip mapM_ (M.elems members) $ withMemberConn (\c -> mapM_ (sendMsg c) choppy)
    return ()
    where
    sendMsg :: Connection -> ByteString -> IO ()
    sendMsg conn = flip onException (putStrLn "You suck")
                 . W.send conn . W.DataMessage . W.Binary


chopList :: ByteString -> [ByteString] -> [ByteString]
chopList header pieces = chopper 0 (hLen + 2) (flbs BS.empty) pieces []
    where
    tlbs = toLazyByteString
    flbs = fromLazyByteString
    hLen = BS.length header
    hBld = flbs header
    packetSize = 2048
    chopper :: Int64 -> Int64 -> Builder -> [ByteString] -> [ByteString] -> [ByteString]
    chopper n accLen acc (x:xs) bs =
        let xLen = BS.length x
            currentSize = xLen + accLen in
        if currentSize < packetSize 
        -- Add to chunk
        then chopper (n + 1) currentSize (acc <> flbs x) xs bs
        -- Start new chunk
        else chopper 1 (hLen + xLen + 2) (flbs x) xs (tlbs (hBld <> fromWord16be (fromIntegral n :: Word16) <> acc) : bs)
    chopper n _ acc _ bs = (tlbs (hBld <> fromWord16be (fromIntegral n :: Word16) <> acc) : bs)