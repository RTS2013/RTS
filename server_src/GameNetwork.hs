{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module GameNetwork where

import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector.Unboxed.Mutable as M
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send)
import Network.Socket.ByteString.Lazy
import Control.Concurrent.STM.TVar
import Control.Monad (forever)
import Data.Binary (decodeOrFail,Binary,get,put,Get)
import Control.Concurrent.STM.TVar
import Control.Monad.STM (atomically)
import System.Timeout (timeout)
import Control.Concurrent (forkIO)
import Data

type Name = String
type TeamId = Int
data HelloMessage = HelloMessage Name TeamId

instance Binary HelloMessage where
    get = do
        name <- get
        team <- get
        return $ HelloMessage name team
    put = undefined

data PlayerConnection = PlayerConnection Name PlayerStatus Socket Socket SockAddr
data ServerState = ServerState (V.Vector Int) [PlayerConnection]

tcpGameServer :: V.Vector Int -> IO ()
tcpGameServer teamCounts = withSocketsDo $ do
    let addrFamily = if isSupportedFamily AF_INET6 then AF_INET6 else AF_INET
    localAddr <- fmap head $ getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) 
                                         (Just "Scrounger") 
                                         (Just "3000")
    sock <- socket addrFamily Stream defaultProtocol
    bind sock (addrAddress localAddr)
    serverStateVar <- newTVarIO $ ServerState teamCounts []
    -- Only accept as many connections as there are potential players
    listen sock $ V.foldl (+) 0 teamCounts
    -- Accept a single player
    let acceptPlayer :: (Socket,SockAddr) -> IO ()
        acceptPlayer (sockTCP,addr) = do
            bind sockTCP addr
            -- Try to validate player. Give up in 5 seconds.
            accepted <- timeout 5000000 $ do
                msg <- recv sockTCP 512
                case decodeOrFail msg of
                    Left _ -> putStrLn "Somebody didn't have a multi-pass"
                    Right (_,_, HelloMessage name team) -> do
                        -- Get next connection
                        sockUDP <- socket addrFamily Datagram defaultProtocol
                        bind sockUDP (addrAddress localAddr)
                        connect sockUDP addr
                        slotExisted <- atomically $ do
                            (ServerState teamCounts playerList) <- readTVar serverStateVar
                            case teamCounts V.!? team of
                                Nothing -> return False
                                Just count -> 
                                    if count > 0 
                                    then writeTVar serverStateVar (ServerState 
                                        (V.modify (\v -> M.write v team (count-1)) teamCounts)
                                        (PlayerConnection name Playing sockTCP sockUDP addr : playerList))
                                        >> return True
                                    else return False
                        if slotExisted
                        then putStrLn $ name ++ " joined team " ++ show team
                        else putStrLn $ name ++ " couldn't join team " ++ show team
            case accepted of
                Just __ -> return ()
                Nothing -> return ()
        -- Loop of accepting new players
        acceptPlayers :: IO ()
        acceptPlayers = do
            connection <- accept sock
            putStrLn $ "Accepted connection from " ++ show (snd connection)
            forkIO $ acceptPlayer connection
            acceptPlayers
    acceptPlayers