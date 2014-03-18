module Chopper where

import qualified Data.ByteString.Lazy as BS
import           Blaze.ByteString.Builder
import           Data.Monoid ((<>))
import           Data.Word (Word16)
import           Data.Int (Int64)

{-
chopList :: Builder -> Int -> [BS.ByteString] -> [BS.ByteString]
chopList bs n (x:xs) = 
    if   BS.length x + BS.length bs > packetSize
    then (toLazyByteString $ headerB <> (fromLazyByteString (encode (fromIntegral n :: Word16)) <> bs)) : chopList BS.empty 0 (x:xs)
    else chopList (fromLazyByteString x <> bs) (n+1) xs
chopList bs n _      = [headerB (fromLazyByteString (encode (fromIntegral n :: Word16)) <> bs)]
-}



chopList :: BS.ByteString -> [BS.ByteString] -> [BS.ByteString]
chopList header pieces = chopper 0 (hLen + 2) (flbs BS.empty) pieces []
    where
    tlbs = toLazyByteString
    flbs = fromLazyByteString
    hLen = BS.length header
    hBld = flbs header
    packetSize = 2048
    -- chopper :: Int64 -> Int64 -> Builder -> [BS.ByteString] -> [BS.ByteString] -> [BS.ByteString]
    chopper n accLen acc (x:xs) bs =
        let xLen = BS.length x
            currentSize = xLen + accLen in
        if currentSize < packetSize 
        -- Add to chunk
        then chopper (n + 1) currentSize (acc <> flbs x) xs bs
        -- Start new chunk
        else chopper 1 (hLen + xLen + 2) (flbs x) xs (tlbs (hBld <> fromStorable (fromIntegral n :: Word16) <> acc) : bs)
    chopper n _ acc _ bs = (tlbs (hBld <> fromStorable (fromIntegral n :: Word16) <> acc) : bs)