{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Trustworthy, DeriveGeneric, OverloadedStrings, FlexibleContexts #-}

module Data where

import Data.IORef (IORef)
import Data.Text (Text)
import Data.Int (Int32)
import Data.List (genericLength)
import Data.Binary (Binary,get,put)
import Data.Text.Binary ()
import Data.Word (Word8,Word16,Word32,Word64)
import Data.HashTable.IO (BasicHashTable)
import Data.IntMap (IntMap)
import Data.Sequence (Seq)
import Data.Array.ST (newArray,readArray,MArray,STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.Generics (Generic)
import GHC.ST (runST,ST)
import Grid.UnboxedGrid (MGrid)
import RIO.Prelude (ReadOnly,ReadWrite,RIO)
import Party (Party,Player)
import Movement (Point(..))

defaultGame :: [Player] -> Game gameS teamS unitS tileS
defaultGame = undefined

type HashTable a = BasicHashTable Int a

type Change gameS teamS unitS tileS =
    Game gameS teamS unitS tileS -> RIO ReadWrite ()

type Behavior gameS teamS unitS tileS behaving = 
    behaving gameS teamS unitS tileS -> 
    RIO ReadOnly (Change gameS teamS unitS tileS)

data Game gameS teamS unitS tileS = Game 
    { gameStep      :: {-# UNPACK #-} !Double
    , gameState     :: !(IORef gameS)
    , gameTiles     :: !(MGrid tileS)
    , gameTime      :: !(IORef Double)
    , gameParty     :: !(Party ControlMessage)
    , gameTeams     :: !(HashTable (Team gameS teamS unitS tileS))
    , gameValues    :: !(IORef (Game gameS teamS unitS tileS -> [(Word8,Word16)]))
    , gameBehaviors :: !(IORef (IntMap (Behavior gameS teamS unitS tileS Game)))
    } 

data Team gameS teamS unitS tileS = Team
    { teamState      :: !teamS
    , teamPlayers    :: ![Player]
    , teamVision     :: !(MGrid Int)
    , teamSpawnCount :: {-# UNPACK #-} !Int -- Incremented with each new unit
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

----------------------------------------------------------------------------
--             # DATA SENT OVER WIRE FROM CLIENT TO SERVER #              --
----------------------------------------------------------------------------

data ControlMessage
    = OrderMsg Orders
    | TeamMsg -- TODO IMPLEMENT
    | PlayerMsg -- TODO IMPLEMENT
    | AllMsg -- TODO IMPLEMENT
    deriving (Generic)

data Orders = Orders !Bool ![Int] !Order deriving (Generic)

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

----------------------------------------------------------------------------
--             # DATA SENT OVER WIRE FROM SERVER TO CLIENT #              --
----------------------------------------------------------------------------

instance Binary (GameFrame gameS teamS unitS tileS) where
    get = undefined
    put (GameFrame delta gram) = do
        put $ doubleToWord delta
        put gram

data GameFrame gameS teamS unitS tileS = GameFrame {-# UNPACK #-} !Double !(ClientDatagram gameS teamS unitS tileS) 

instance Binary (ClientDatagram gameS teamS unitS tileS) where
    get = undefined
    put (UnitDatagram xs) = do
        put (fromIntegral $ length xs :: Word8)
        mapM_ put xs
    put (TeamDatagram xs) = do
        put (fromIntegral $ length xs :: Word8)
        mapM_ put xs
    put (TerrainDatagram xs) = do
        put (fromIntegral $ length xs :: Word8)
        mapM_ put xs

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

data Terrain = Terrain
    { terrainID :: {-# UNPACK #-} !Word16
    , terrainX  :: {-# UNPACK #-} !Word16
    , terrainY  :: {-# UNPACK #-} !Word16
    , terrainZ  :: {-# UNPACK #-} !Word16
    } deriving (Generic)


data SFX = SFX
    { sfxID :: {-# UNPACK #-} !Word16
    , sfxX  :: {-# UNPACK #-} !Word16
    , sfxY  :: {-# UNPACK #-} !Word16
    , sfxZ  :: {-# UNPACK #-} !Word16
    } deriving (Generic)


data TargetFX = TargetFX
    { targetfxID   :: {-# UNPACK #-} !Word16
    , targetfxUID  :: {-# UNPACK #-} !Int32
    , targetfxteam :: {-# UNPACK #-} !Word8
    } deriving (Generic)


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

instance Binary Terrain
instance Binary SFX
instance Binary TargetFX
instance Binary LineFX
