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
import           Data.Vector.Unboxed (Vector)
import           Data.Vector.Mutable (IOVector)
import           Data.Word (Word8, Word16, Word32, Word64)
import           GHC.Generics (Generic)
import           GHC.ST (runST, ST)
import qualified Grid.Boxed   as G
import qualified Grid.Unboxed as UG
import           KDTree (KDTree)
import           MIO.Privileges

type Ref = IORef
type HashTable a = BasicHashTable Int a

data Game g u t = Game 
    { gameStepRef      :: !(Ref Double)
    , gameStateRef     :: !(Ref g)
    , gameKDTRef       :: !(Ref (KDTree Float (Unit g u t)))
    , gameTilesRef     :: !(Ref (UG.Grid (Int,Bool)))
    , gameCornersRef   :: !(Ref (G.Grid (Vector (Int,Int))))
    , gameTeamsVec     :: !(IOVector (Team g u t))
    , gameBehaviorsRef :: !(Ref (IntMap (Behavior (Game g u t, Float) (Change (Game g u t) ()))))
    } 

data Team g u t = Team
    { teamID         :: !Int
    , teamState      :: !(Ref t)
    , teamSpawnCount :: !(Ref Int) -- Incremented with each new unit
    , teamUnits      :: !(HashTable (Unit g u t))
    , teamBehaviors  :: !(Ref (IntMap (Behavior (Game g u t, Float, Team g u t) (Change (Game g u t) ()))))
    } 

data Unit g u t = Unit
    { unitModState    :: !u
    , unitAnimation   :: !Int
    , unitOrders      :: !(Seq Order)
    , unitMoveState   :: !Vec4
    , unitStaticState :: !(StaticState g u t)
    , unitBehaviors   :: !(IntMap (Behavior (Game g u t, Float, Unit g u t) (Change (Game g u t) ())))
    } 

data Vec4 = Vec4
    { _v1 :: !Float
    , _v2 :: !Float
    , _v3 :: !Float
    , _v4 :: !Float
    } 

data StaticState g u t = StaticState
    { unitID    :: !Int
    , unitTeam  :: !Int
    , unitType  :: !Int
    , speedBase :: !Float
    , speed     :: !Float
    , weight    :: !Float
    , radius    :: !Float
    , unitVals  :: !(Unit g u t -> [(Word8,Word16)])
    }

instance Eq (Unit g u t) where
    a == b = unitTeam (unitStaticState a) == unitTeam (unitStaticState b) 
             && unitID (unitStaticState a) == unitID (unitStaticState b)

instance Ord (Unit g u t) where
    compare a b = compare (unitID (unitStaticState a), unitTeam (unitStaticState a)) 
                          (unitID (unitStaticState b), unitTeam (unitStaticState b))

instance Show (Unit g u t) where
    show u = let ms = unitMoveState u in
        show (_v1 ms, _v2 ms)

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

data Point = Point {_X,_Y,_Z :: !Float}

data Orders = Orders !Bool ![Int] !Order
    deriving (Generic)

data Order 
    = Standby
    | Move      !Float -- x
                !Float -- y
                ![(Float,Float)] -- Path
    | Assault   !Float -- x
                !Float -- y
                ![(Float,Float)] -- Path
    | Target    !Int !Int
                ![(Float,Float)] -- Path
    | Hold      !Float -- x
                !Float -- y
    | Patrol    !Float -- xa
                !Float -- ya
                !Float -- xb
                !Float -- yb
                ![(Float,Float)] -- Path
    | Invoke    !Int {- Ability ID -}
    | InvokeAt  !Int {- Ability ID -} 
                !Float -- x
                !Float -- y
                ![(Float,Float)] -- Path
    | InvokeOn  !Int -- Ability ID
                !Int -- Team ID
                !Int -- Unit ID
                ![(Float,Float)] -- Path
    deriving (Generic)

instance Binary Orders
instance Binary Order

----------------------------------------------------------------------------
--             # DATA SENT OVER WIRE FROM SERVER TO CLIENT #              --
----------------------------------------------------------------------------

buildUnit :: Unit g u t -> Builder
buildUnit u = buildCore <> buildVals
    where
    stat = unitStaticState u
    xyz = unitMoveState u
    vals = (unitVals stat) u
    buildCore = fromWrite $
                writeInt32be  (fromIntegral $ unitID stat    ) <>
                writeWord8    (fromIntegral $ unitTeam stat  ) <>
                writeWord8    (fromIntegral $ unitAnimation u) <>
                writeWord16be (fromIntegral $ unitType stat  ) <>
                writeWord16be (coordTo16    $ _v1 xyz        ) <>
                writeWord16be (coordTo16    $ _v2 xyz        ) <>
                writeWord16be (coordTo16    $ _v3 xyz        ) <>
                writeWord8    (faceTo8      $ _v4 xyz        ) <>
                writeWord8    (genericLength vals            )
    buildVals = mconcat $ map (\(k,v) -> fromWrite $ writeWord8 k <> writeWord16be v) vals
    faceTo8 :: Float -> Word8
    faceTo8 a | a < 0 = floor $ (2*pi + a) / (2*pi) * 256
              | True  = floor $ a / (2*pi) * 256
    coordTo16 :: Float -> Word16
    coordTo16 a = floor $ a * 64

-- Datagram sent to client
data ClientDatagram g u t
    = UnitDatagram     ![Unit g u t]
    | TeamDatagram     ![(Word8,Word16)]
    | TerrainDatagram  ![Terrain]
    | LineFXDatagram   ![LineFX]
    | SFXDatagram      ![SFX]
    | TargetFXDatagram ![TargetFX]
    | MessageDatagram  !Text
    | YourPlaceInLife  !Word8 -- Team ID
                       !Word16 -- X Coord
                       !Word16 -- Y Coord

instance Binary Point where
    put = undefined
    get = do
        x <- get :: Get Word32
        y <- get :: Get Word32
        z <- get :: Get Word32
        return $ Point (wordToFloat x) (wordToFloat y) (wordToFloat z)

instance Binary Terrain

data Terrain = Terrain
    { terrainID :: !Word16
    , terrainX  :: !Word16
    , terrainY  :: !Word16
    , terrainZ  :: !Word16
    } deriving (Generic)

instance Binary SFX

data SFX = SFX
    { sfxID :: !Word16
    , sfxX  :: !Word16
    , sfxY  :: !Word16
    , sfxZ  :: !Word16
    } deriving (Generic)

instance Binary TargetFX

data TargetFX = TargetFX
    { targetfxID   :: !Word16
    , targetfxUID  :: !Int32
    , targetfxteam :: !Word8
    } deriving (Generic)

instance Binary LineFX

data LineFX = LineFX
    { linefxID :: !Word16
    , linefxXA :: !Word16
    , linefxYA :: !Word16
    , linefxZA :: !Word16
    , linefxXB :: !Word16
    , linefxYB :: !Word16
    , linefxZB :: !Word16
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