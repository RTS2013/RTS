{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Trustworthy, DeriveGeneric, OverloadedStrings, FlexibleContexts #-}

module Data where

import Data.IORef (IORef)
import Data.Text (Text)
import Data.Int (Int32)
import Data.List (genericLength)
import Data.Binary (Binary,get,put)
import Data.Word (Word8,Word16,Word32,Word64)
import Data.HashTable.IO (BasicHashTable)
import Data.IntMap (IntMap)
import Data.Sequence (Seq)
import Data.Array.ST (newArray,readArray,MArray,STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.Generics (Generic)
import GHC.ST (runST,ST)
import Grid.UnboxedGrid (MGrid)
import RIO.Privileges (ReadOnly,ReadWrite,RIO)
import Party (Party,Player)
import Movement (Point(..))

type Ref = IORef
type HashTable a = BasicHashTable Int a

type Behavior gameS teamS unitS tileS behaving = 
    behaving gameS teamS unitS tileS -> 
    RIO ReadOnly (RIO ReadWrite ())

data Game gameS teamS unitS tileS = Game 
    { gameStep      :: !(Ref Double)
    , gameState     :: !(Ref gameS)
    , gameTiles     :: !(MGrid tileS)
    , gameParty     :: !(Party ControlMessage)
    , gameTeams     :: !(HashTable (Team gameS teamS unitS tileS))
    , gameValues    :: !(Ref (Game gameS teamS unitS tileS -> [(Word8,Word16)]))
    , gameBehaviors :: !(Ref (IntMap (Behavior gameS teamS unitS tileS Game)))
    } 

data Team gameS teamS unitS tileS = Team
    { teamState      :: !(Ref teamS)
    , teamPlayers    :: ![Player]
    , teamVision     :: !(MGrid Int)
    , teamSpawnCount :: !(Ref Int) -- Incremented with each new unit
    , teamUnits      :: !(HashTable (Unit gameS teamS unitS tileS))
    , teamValues     :: !(Team gameS teamS unitS tileS -> [(Word8,Word16)])
    , teamBehaviors  :: !(IntMap (Behavior gameS teamS unitS tileS Team))
    } 

data Unit gameS teamS unitS tileS = Unit
    { unitState        :: !unitS
    , unitID           :: {-# UNPACK #-} !Int
    , unitTeam         :: {-# UNPACK #-} !Int
    , unitType         :: {-# UNPACK #-} !Int
    , unitAnimation    :: {-# UNPACK #-} !Int
    , unitOrders       :: !(Seq Order)
    , unitMoveState    :: !MoveState
    , unitValues       :: !(Unit gameS teamS unitS tileS -> [(Word8,Word16)])
    , unitBehaviors    :: !(IntMap (Behavior gameS teamS unitS tileS Unit))
    } 

data MoveState = MoveState
    { positionX :: {-# UNPACK #-} !Float
    , positionY :: {-# UNPACK #-} !Float
    , positionZ :: {-# UNPACK #-} !Float
    , angle     :: {-# UNPACK #-} !Float
    , speed     :: {-# UNPACK #-} !Float
    , angleRate :: {-# UNPACK #-} !Float
    , speedMax  :: {-# UNPACK #-} !Float
    , speedRate :: {-# UNPACK #-} !Float
    , weight    :: {-# UNPACK #-} !Float
    , radius    :: {-# UNPACK #-} !Float
    } 

----------------------------------------------------------------------------
--             # DATA SENT OVER WIRE FROM CLIENT TO SERVER #              --
----------------------------------------------------------------------------

data ControlMessage
    = OrderMsg Orders
    | TeamMsg -- TODO IMPLEMENT
    | PlayerMsg -- TODO IMPLEMENT
    | AllMsg -- TODO IMPLEMENT
    | Ready -- TODO IMPLEMENT
    deriving (Generic)

instance Binary ControlMessage

data Orders = Orders !Bool ![Int] !Order
    deriving (Generic)

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

instance Binary Orders
instance Binary Order

----------------------------------------------------------------------------
--             # DATA SENT OVER WIRE FROM SERVER TO CLIENT #              --
----------------------------------------------------------------------------

instance Binary (Unit gameS teamS unitS tileS) where
    get = undefined
    put u = do
        put (fromIntegral $ unitID u        :: Int32)
        put (fromIntegral $ unitTeam u      :: Word8)
        put (fromIntegral $ unitAnimation u :: Word8)
        put (fromIntegral $ unitType u      :: Word16)
        let xyz = unitMoveState u
        put (coordTo16 $ positionX xyz :: Word16)
        put (coordTo16 $ positionY xyz :: Word16)
        put (coordTo16 $ positionZ xyz :: Word16)
        put (faceTo8 $ angle xyz :: Word8)
        let vals = (unitValues u) u
        put (genericLength vals :: Word8) -- Length of list
        mapM_ put vals                    -- (Key,Val) Pairs
        where
        faceTo8 :: Float -> Word8
        faceTo8 a | a < 0 = floor $ (2*pi + a) / (2*pi) * 256
                  | True  = floor $ a / (2*pi) * 256
        coordTo16 :: Float -> Word16
        coordTo16 a = floor $ a * 64

instance Binary (GameFrame gameS teamS unitS tileS) where
    get = undefined
    put (GameFrame delta gram) = do
        put $ doubleToWord delta
        put gram

data GameFrame gameS teamS unitS tileS = GameFrame {-# UNPACK #-} !Double !(ClientDatagram gameS teamS unitS tileS) 

instance Binary (ClientDatagram gameS teamS unitS tileS) where
    get = undefined
    put (UnitDatagram xs) = do
        put (0 :: Word8)
        put (fromIntegral $ length xs :: Word8)
        mapM_ put xs
    put (TeamDatagram xs) = do
        put (1 :: Word8)
        put (fromIntegral $ length xs :: Word8)
        mapM_ put xs
    put (TerrainDatagram xs) = do
        put (2 :: Word8)
        put (fromIntegral $ length xs :: Word8)
        mapM_ put xs
    put (LineFXDatagram xs) = do
        put (3 :: Word8)
        put (fromIntegral $ length xs :: Word8)
        mapM_ put xs
    put (SFXDatagram xs) = do
        put (4 :: Word8)
        put (fromIntegral $ length xs :: Word8)
        mapM_ put xs
    put (TargetFXDatagram xs) = do
        put (5 :: Word8)
        put (fromIntegral $ length xs :: Word8)
        mapM_ put xs
    put (MessageDatagram txt) = put txt
    put (YourPlaceInLife tid x y) = put tid >> put x >> put y

-- Datagram sent to client
data ClientDatagram gameS teamS unitS tileS
    = UnitDatagram     ![Unit gameS teamS unitS tileS]
    | TeamDatagram     ![(Word8,Word16)]
    | TerrainDatagram  ![Terrain]
    | LineFXDatagram   ![LineFX]
    | SFXDatagram      ![SFX]
    | TargetFXDatagram ![TargetFX]
    | MessageDatagram  !Text
    | YourPlaceInLife  {-# UNPACK #-} !Word8 -- Team ID
                       {-# UNPACK #-} !Word16 -- X Coord
                       {-# UNPACK #-} !Word16 -- Y Coord

instance Binary Terrain

data Terrain = Terrain
    { terrainID :: {-# UNPACK #-} !Word16
    , terrainX  :: {-# UNPACK #-} !Word16
    , terrainY  :: {-# UNPACK #-} !Word16
    , terrainZ  :: {-# UNPACK #-} !Word16
    } deriving (Generic)

instance Binary SFX

data SFX = SFX
    { sfxID :: {-# UNPACK #-} !Word16
    , sfxX  :: {-# UNPACK #-} !Word16
    , sfxY  :: {-# UNPACK #-} !Word16
    , sfxZ  :: {-# UNPACK #-} !Word16
    } deriving (Generic)

instance Binary TargetFX

data TargetFX = TargetFX
    { targetfxID   :: {-# UNPACK #-} !Word16
    , targetfxUID  :: {-# UNPACK #-} !Int32
    , targetfxteam :: {-# UNPACK #-} !Word8
    } deriving (Generic)

instance Binary LineFX

data LineFX = LineFX
    { linefxID :: {-# UNPACK #-} !Word16
    , linefxXA :: {-# UNPACK #-} !Word16
    , linefxYA :: {-# UNPACK #-} !Word16
    , linefxZA :: {-# UNPACK #-} !Word16
    , linefxXB :: {-# UNPACK #-} !Word16
    , linefxYB :: {-# UNPACK #-} !Word16
    , linefxZB :: {-# UNPACK #-} !Word16
    } deriving (Generic)


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
cast x = newArray (0 :: Word64, 0) x >>= castSTUArray >>= flip readArray 0