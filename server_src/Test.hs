{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, DeriveGeneric, OverloadedStrings #-}

module Test where

import Data.Binary (Binary)
import Data.Word
import GHC.Generics (Generic)
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST, ST)
import Party

{-
main :: IO ()
main = do
    putStrLn "Waiting for connections..."
    _ <- openDoors 4444 [5] :: IO (Party ())
    let doForever = getLine >>= \s -> if s == "exit" then return () else putStrLn s >> doForever
    doForever
-}

{-
main = do
    print $ (decode $ encode False :: Word8)
    print $ BS.length $ encode $ OrderMessage True 0 0 0 [0]
    print $ BS.length $ BS.fromChunks $ [encodeUtf8 "a",encodeUtf8 "b"]
-}

wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)

doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

data ClientMessage 
    = OrderMessage  
                   !Bool  -- True = Add to commands, False = Replace commands
    {-# UNPACK #-} !Word64 -- Type Id
    {-# UNPACK #-} !Word32 -- X coord
    {-# UNPACK #-} !Word32 -- Y coord
                   ![Word64] -- List of actors to give command to
    | Invalid
    deriving (Generic)

instance Binary ClientMessage