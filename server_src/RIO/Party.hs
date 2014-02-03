{-# LANGUAGE Trustworthy #-}

module RIO.Party
( sendToPlayer
, sendToPlayers
) where 

import qualified Data.ByteString.Lazy (ByteString)
import qualified Party as P
import Data.Binary (Binary)
import RIO.Prelude
import RIO.RIO

sendToPlayer :: (Binary a) => ByteString -> [a] -> P.Player -> RIO ReadWrite ()
sendToPlayer a b c = RIO $! P.sendToPlayer a b c

sendToPlayers :: (Binary a) => ByteString -> [a] -> [P.Player] -> RIO ReadWrite ()
sendToPlayers a b c = RIO $! P.sendToPlayers a b c