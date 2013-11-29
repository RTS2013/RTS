module RTSNetwork (connectPlayers,sendDatagram,serverCommand) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as SB
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as V
import Control.Concurrent (forkIO,killThread,myThreadId,ThreadId)
import Control.Concurrent.STM.TVar
import Control.Monad.STM (atomically)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy (recv)
import Network.Socket.ByteString (sendMany)
import System.Timeout (timeout)
import Data.Binary (decodeOrFail,Binary,get,put,Get,encode)
import Data.Word (Word8)
import Data




connectPlayers :: TVar (V.Vector Int) -> TVar [Player] -> TVar [ClientMessage] -> IO ThreadId
connectPlayers teamsVar playersVar messagesVar = forkIO $ withSocketsDo $ do
    teams <- atomically $ readTVar teamsVar
    let port = Just "3000"
    let hint = Just $ defaultHints {addrFlags = [AI_PASSIVE]}
    let service = Nothing
    serverAddress <- fmap head $ getAddrInfo hint service port
    sock <- socket (addrFamily serverAddress) Stream defaultProtocol
    bind sock (addrAddress serverAddress)
    listen sock $ V.sum teams

    let -- Accept messages from player and add them to message box
        acceptMessages :: Player -> TVar [ClientMessage] -> IO ()
        acceptMessages p msgsVar = do
            msg <- recv (player_tcpSock p) 512
            case decodeOrFail msg of
                Left _ -> do 
                    putStrLn $ player_name p ++ " sent a bad message."
                    acceptMessages p msgsVar
                Right (_,_,msg) -> do
                    atomically $ readTVar msgsVar >>= \msgs -> writeTVar msgsVar (msg:msgs)
                    acceptMessages p msgsVar

    let -- Connect an individual player to the client message box
        connectPlayer :: (Socket,SockAddr) -> ThreadId -> IO ()
        connectPlayer (sockTCP,playerAddress) playerThread = do
            sockUDP <- socket (addrFamily serverAddress) Datagram defaultProtocol
            -- Timeout after 5 seconds
            accepted <- timeout 500 $! do
                -- Get hello message from potential player
                helloMessage <- recv sockTCP 512
                -- Verify hello message
                case decodeOrFail helloMessage of
                    -- Bad hello message
                    Left _ -> close sockTCP >> putStrLn (show playerAddress ++ " sent a bad hello message.")
                    -- Good hello message
                    Right (_,_,HelloMessage team name secret) -> do
                        let player = Player team name secret sockTCP sockUDP Playing playerThread
                        -- Did the player get on the desired team?
                        joinedTeam <- atomically $ do
                            teams <- readTVar teamsVar
                            -- Does team even exist?
                            case teams V.!? team of
                                -- Team doesn't exist
                                Nothing -> return False
                                -- Team exists, check for available slot
                                Just count -> if count <= 0 then return False else do
                                    -- Available slot on team, atomically add player to team
                                    writeTVar teamsVar $ V.modify (\teams -> M.write teams team (count-1)) teams
                                    readTVar playersVar >>= \players -> writeTVar playersVar (player:players)
                                    return True
                        if joinedTeam
                        then do
                            connect sockUDP playerAddress 
                            putStrLn ""
                            acceptMessages player messagesVar
                        else do
                            putStrLn $ show playerAddress ++ " failed to join a team."
                            close sockTCP >> close sockUDP
            case accepted of
                Nothing -> do
                    putStrLn $ show playerAddress ++ " timed out."
                    close sockTCP >> close sockUDP
                Just __ -> return ()

    let -- Loop and connect players to message box
        connectPlayers :: IO ()
        connectPlayers = do
            connection <- accept sock
            putStrLn $ show (snd connection) ++ " connected to server."
            myThreadId >>= forkIO . connectPlayer connection
            connectPlayers
    connectPlayers




sendDatagram :: Binary a => Word8 -> Socket -> [a] -> IO ()
sendDatagram header sock xs = sendGram sock $ map (B.toStrict . encode) xs
    where

    split512 :: Int -> ([SB.ByteString],[SB.ByteString],Word8) -> ([SB.ByteString],[SB.ByteString],Word8)
    split512 n (x:xs,ys,c) = let len = SB.length x + n in
        if len <= 512 then split512 len (xs,x:ys,c+1) else (x:xs,ys,c)
    split512 _ other = other

    sendGram :: Socket -> [SB.ByteString] -> IO ()
    sendGram sock ents = do
        let (leftOvers,gram,c) = split512 0 (ents,[],0)
        if null gram
        then return ()
        else sendMany sock (SB.pack [header,c] : gram) >> sendGram sock leftOvers




serverCommand :: TVar (V.Vector Int) -> TVar [Player] -> TVar [ClientMessage] -> IO ()
serverCommand teamsVar playersVar messagesVar = do
    command <- getLine
    case command of
        ".start" -> return ()
        ".list" -> do
            clean
            players <- atomically $ readTVar playersVar
            if null players
            then do
                putStrLn "There are no players..."
                loop
            else do
            mapM_ (putStrLn . player_name) players
            loop
        -- Kick player
        ('.':'k':'i':'c':'k':' ':name) -> do
            kickPlayer name
            loop
        -- Specify name for kick command
        ('.':'k':'i':'c':'k':_) -> do
            putStrLn "You must enter a name."
            loop
        ".?" -> do
            clean
            putStrLn "    .list         Lists the players currently connected."
            putStrLn "    .start        Starts the game with the current player pool."
            putStrLn "    .kick <name>  Kick player from the game."
            putStrLn "    .?            List all available commands."
            loop
        cmd@(x:xs) -> case x of
            '.' -> do
                clean
                putStrLn $ cmd ++ " is not a command. Type '.?' for a list of commands."
                loop
        _ -> loop
    where
    clean = kickPlayer ""
    loop = serverCommand teamsVar playersVar messagesVar
    kickPlayer :: String -> IO ()
    kickPlayer badPlayer = do
        players <- atomically $ do 
            players <- readTVar playersVar
            writeTVar playersVar []
            return players
        goodPlayers <- filterPlayers badPlayer players
        atomically $ do
            players <- readTVar playersVar
            writeTVar playersVar (players ++ goodPlayers)
    filterPlayers :: String -> [Player] -> IO [Player]
    filterPlayers badPlayer (player:players) = do
        let name = player_name player
        let sockTCP = player_tcpSock player
        let sockUDP = player_udpSock player
        isConnTCP <- isConnected sockTCP
        isConnUDP <- isConnected sockUDP
        if isConnTCP && isConnUDP && name /= badPlayer
        then do
            xs <- filterPlayers badPlayer players
            return $ player:xs
        else do
            close sockTCP
            close sockUDP
            putStrLn $ name ++ " has left the game."
            atomically $ do
                teams <- readTVar teamsVar
                let team = player_team player
                let count = teams V.! team
                writeTVar teamsVar $ V.modify (\teams -> M.write teams team (count+1)) teams
            filterPlayers badPlayer players
    filterPlayers _ players = return players