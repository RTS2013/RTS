{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Trustworthy, DeriveGeneric, OverloadedStrings, FlexibleContexts #-}

module Data where

import Data.Text (Text)
import Data.Int (Int64)
import Data.List (genericLength)
import Data.Binary (Binary,get,put)
import Data.Text.Binary ()
import Data.Word (Word8,Word16,Word64)
import Data.HashTable.IO (BasicHashTable)
import Data.Sequence (Seq)
import Data.Array.ST (newArray,readArray,MArray,STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.Generics (Generic)
import GHC.ST (runST,ST)
import Grid.UnboxedGrid (MGrid,MGrid)
import RIO.Prelude (ReadOnly,ReadWrite,RIO)
import Party (Party,Player)
import Movement (Point(..),Bulk(..))

defaultGame :: [Player] -> Game gameS teamS unitS tileS
defaultGame = undefined

type Change gameS teamS unitS tileS =
    Game gameS teamS unitS tileS -> RIO ReadWrite (Game gameS teamS unitS tileS)

type Behavior gameS teamS unitS tileS behaving = 
    behaving gameS teamS unitS tileS -> 
    RIO ReadOnly (Change gameS teamS unitS tileS)

data Game gameS teamS unitS tileS = Game
    { gameState     :: !gameS
    , gameTiles     :: !(MGrid tileS)
    , gameStep      :: !Int64
    , gameParty     :: !(Party ControlMessage)
    , gameTeams     :: !(BasicHashTable Int (Team gameS teamS unitS tileS))
    , gameBehaviors :: !(BasicHashTable Int (Behavior gameS teamS unitS tileS Game))
    } 

data Team gameS teamS unitS tileS = Team
    { teamState      :: !teamS
    , teamPlayers    :: ![Player]
    , teamVision     :: !(MGrid Int)
    , teamSpawnCount :: {-# UNPACK #-} !Int -- Incremented with each new unit
    , teamUnits      :: !(BasicHashTable Int (Unit gameS teamS unitS tileS))
    , teamBehaviors  :: !(BasicHashTable Int (Behavior gameS teamS unitS tileS Team))
    , teamValues     :: !(Team gameS teamS unitS tileS -> [(Word8,Word16)])
    } 

data Unit gameS teamS unitS tileS = Unit
    { unitState     :: !unitS
    , unitID        :: {-# UNPACK #-} !Int 
    , unitTeam      :: {-# UNPACK #-} !Int
    , unitType      :: {-# UNPACK #-} !Int
    , unitAnim      :: {-# UNPACK #-} !Int
    , unitPoint     :: !Point
    , unitBulk      :: !Bulk
    , unitOrders    :: !(Seq Order)
    , unitBehaviors :: !(BasicHashTable Int (Behavior gameS teamS unitS tileS Unit))
    , unitValues    :: !(Unit gameS teamS unitS tileS -> [(Word8,Word16)])
    } 

----------------------------------------------------------------------------
--                       # DATA SENT OVER WIRE #                          --
----------------------------------------------------------------------------

data ControlMessage
    = OrderMsg Orders
    | TeamMsg
    | PlayerMsg
    | AllMsg
    deriving (Generic)

data Orders = Orders !Bool [Int] !Order deriving (Generic)

data Order 
    = Standby
    | Move    !Point
    | Assault !Point
    | Target  {-# UNPACK #-} !Word8 {-# UNPACK #-} !Int
    | Hold    !Point
    | Patrol  !Point !Point 
    | Invoke {-# UNPACK #-} !Word16 {- Ability ID -}
    | InvokeAt {-# UNPACK #-} !Word16 {- Ability ID -} !Point
    | InvokeOn 
    {-# UNPACK #-} !Word16 -- Ability ID
    {-# UNPACK #-} !Word8  -- Team ID
    {-# UNPACK #-} !Int -- Unit ID
    deriving (Generic)

data GameFrame gameS teamS unitS tileS = GameFrame {-# UNPACK #-} !Int64 !(ClientDatagram gameS teamS unitS tileS) deriving (Generic)

-- Datagram sent to client
data ClientDatagram gameS teamS unitS tileS
    = UnitDatagram     ![Unit gameS teamS unitS tileS]
    | TeamDatagram     ![(Word8,Word16)]
    | TerrainDatagram  ![Terrain]
    | LineFXDatagram   ![LineFX]
    | SFXDatagram      ![SFX]
    | TargetFXDatagram ![TargetFX]
    | MessageDatagram  !Text
    deriving (Generic)


instance Binary (Unit gameS teamS unitS tileS) where
    get = undefined
    put u = do
        put (unitID u :: Int)
        put (fromIntegral $ unitTeam u :: Word8)
        put (fromIntegral $ unitAnim u :: Word8)
        put (fromIntegral $ unitType u :: Word16)
        let xyz = unitPoint u
        put (coordTo16 $ xCoord xyz :: Word16)
        put (coordTo16 $ yCoord xyz :: Word16)
        put (coordTo16 $ zCoord xyz :: Word16)
        put (faceTo8 $ facing $ unitBulk u :: Word8)
        let vals = (unitValues u) u
        put (genericLength vals :: Word8) -- Length of list
        mapM_ put vals                    -- (Key,Val) Pairs
        where
        faceTo8 :: Float -> Word8
        faceTo8 a | a < 0 = floor $ (2*pi + a) / (2*pi) * 256
                  | True  = floor $ a / (2*pi) * 256
        coordTo16 :: Float -> Word16
        coordTo16 a = floor $ a * 64

data Terrain = Terrain
    { terrainId :: {-# UNPACK #-} !Word16
    , terrainX  :: {-# UNPACK #-} !Word16
    , terrainY  :: {-# UNPACK #-} !Word16
    , terrainZ  :: {-# UNPACK #-} !Word16
    } deriving (Generic)


data SFX = SFX
    { sfxId :: {-# UNPACK #-} !Word16
    , sfxX  :: {-# UNPACK #-} !Word16
    , sfxY  :: {-# UNPACK #-} !Word16
    , sfxZ  :: {-# UNPACK #-} !Word16
    } deriving (Generic)


data TargetFX = TargetFX
    { targetfxId   :: {-# UNPACK #-} !Word16
    , targetfxUID  :: {-# UNPACK #-} !Int
    , targetfxteam :: {-# UNPACK #-} !Word8
    } deriving (Generic)


data LineFX = LineFX
    { linefxId :: {-# UNPACK #-} !Word16
    , linefxXA :: {-# UNPACK #-} !Word16
    , linefxYA :: {-# UNPACK #-} !Word16
    , linefxZA :: {-# UNPACK #-} !Word16
    , linefxXB :: {-# UNPACK #-} !Word16
    , linefxYB :: {-# UNPACK #-} !Word16
    , linefxZB :: {-# UNPACK #-} !Word16
    } deriving (Generic)


wordToFloat :: Int -> Float
wordToFloat x = runST (cast x)

floatToWord :: Float -> Int
floatToWord x = runST (cast x)

wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)

doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Word64, 0) x >>= castSTUArray >>= flip readArray 0

instance Binary ControlMessage
instance Binary Orders
instance Binary Order
instance Binary (GameFrame gameS teamS unitS tileS)
instance Binary (ClientDatagram gameS teamS unitS tileS)
instance Binary Terrain
instance Binary SFX
instance Binary TargetFX
instance Binary LineFX