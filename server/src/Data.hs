{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Trustworthy, DeriveGeneric, OverloadedStrings, FlexibleContexts #-}

module Data where

import           Blaze.ByteString.Builder
import           Data.Array.ST (newArray, readArray, MArray, STUArray)
import           Data.Array.Unsafe (castSTUArray)
import           Data.Binary (Binary, Get, put, get)
import           Data.HashTable.IO (BasicHashTable)
import           Data.Int (Int32)
import           Data.IntMap (IntMap)
import           Data.IORef (IORef)
import           Data.List (genericLength)
import           Data.Monoid (mconcat, (<>))
import           Data.Sequence (Seq)
import           Data.Text (Text)
import           Data.Vector.Mutable (IOVector)
import           Data.Word (Word8, Word16, Word32, Word64)
import           GHC.Generics (Generic)
import           GHC.ST (runST, ST)
import           Grid.UnboxedMutable (Grid)
import           KDT (KDT)
import           MIO.Privileges
import           Party (Party,Player)

type Ref = IORef
type HashTable a = BasicHashTable Int a

data Game gameS teamS unitS tileS = Game 
    { gameStep      :: !(Ref Double)
    , gameState     :: !(Ref gameS)
    , gameKDT       :: !(Ref (KDT Float (Unit gameS teamS unitS tileS)))
    , gameTiles     :: !(Grid tileS)
    , gameParty     :: !(Party ControlMessage)
    , gameTeams     :: !(IOVector (Team gameS teamS unitS tileS))
    , gameBehaviors :: !(Ref (IntMap (Behavior () (Change ()))))
    } 

data Team gameS teamS unitS tileS = Team
    { teamID         :: {-# UNPACK #-} !Int
    , teamState      :: !(Ref teamS)
    , teamVision     :: !(Grid Int)
    , teamSpawnCount :: !(Ref Int) -- Incremented with each new unit
    , teamUnits      :: !(HashTable (Unit gameS teamS unitS tileS))
    , teamBehaviors  :: !(Ref (IntMap (Behavior (Team gameS teamS unitS tileS) (Change ()))))
    } 

data Unit gameS teamS unitS tileS = Unit
    { unitState        :: !unitS
    , unitID           :: {-# UNPACK #-} !Int
    , unitTeam         :: {-# UNPACK #-} !Int
    , unitType         :: {-# UNPACK #-} !Int
    , unitAnimation    :: {-# UNPACK #-} !Int
    , unitOrders       :: !(Seq Order)
    , unitMoveState    :: !MoveState
    , unitMoveStats    :: !MoveStats
    , unitValues       :: !(Unit gameS teamS unitS tileS -> [(Word8,Word16)])
    , unitBehaviors    :: !(IntMap (Behavior (Unit gameS teamS unitS tileS) (Change ())))
    } 

data Point = Point {_X,_Y,_Z :: {-# UNPACK #-} !Float}

instance Eq (Unit gameS teamS unitS tileS) where
    a == b = unitTeam a == unitTeam b && unitID a == unitID b

instance Ord (Unit gameS teamS unitS tileS) where
    compare a b = compare (unitID a, unitTeam a) (unitID b, unitTeam b)

instance Show (Unit gameS teamS unitS tileS) where
    show u = let ms = unitMoveState u in
        show (msX ms, msY ms)

data MoveState = MoveState
    { msX   :: {-# UNPACK #-} !Float
    , msY   :: {-# UNPACK #-} !Float
    , msZ   :: {-# UNPACK #-} !Float
    , angle :: {-# UNPACK #-} !Float
    , speed :: {-# UNPACK #-} !Float
    } 

data MoveStats = MoveStats
    { angleRate :: {-# UNPACK #-} !Float
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

instance Binary Orders where
instance Binary Order

----------------------------------------------------------------------------
--             # DATA SENT OVER WIRE FROM SERVER TO CLIENT #              --
----------------------------------------------------------------------------

buildUnit :: Unit gameS teamS unitS tileS -> Builder
buildUnit u = buildCore <> buildVals
    where
    xyz = unitMoveState u
    vals = (unitValues u) u
    buildCore = fromWrite $
                writeInt32be  (fromIntegral $ unitID u       ) <>
                writeWord8    (fromIntegral $ unitTeam u     ) <>
                writeWord8    (fromIntegral $ unitAnimation u) <>
                writeWord16be (fromIntegral $ unitType u     ) <>
                writeWord16be (coordTo16    $ msX xyz        ) <>
                writeWord16be (coordTo16    $ msY xyz        ) <>
                writeWord16be (coordTo16    $ msZ xyz        ) <>
                writeWord8    (faceTo8      $ angle xyz      ) <>
                writeWord8    (genericLength vals            )
    buildVals = mconcat $ map (\(k,v) -> fromWrite $ writeWord8 k <> writeWord16be v) vals
    faceTo8 :: Float -> Word8
    faceTo8 a | a < 0 = floor $ (2*pi + a) / (2*pi) * 256
              | True  = floor $ a / (2*pi) * 256
    coordTo16 :: Float -> Word16
    coordTo16 a = floor $ a * 64

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

instance Binary Point where
    put = undefined
    get = do
        x <- get :: Get Word32
        y <- get :: Get Word32
        z <- get :: Get Word32
        return $ Point (wordToFloat x) (wordToFloat y) (wordToFloat z)

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