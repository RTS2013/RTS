{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Trustworthy, DeriveGeneric #-}

module Data where

import Data.List (genericLength)
import Control.Applicative ((<$>),(<*>))
import GHC.Generics (Generic)
import Data.Binary (Binary,get,put,getWord8)
import Data.Vector (Vector)
import Data.Text.Binary ()
import Data.Word (Word8,Word16)
import Data.IntMap.Strict (IntMap)
import Data.Sequence (Seq)
import Grid.UnboxedGrid (MGrid,MGrid)
import GoodTimes (Player)
import Movement (Point(..),Bulk(..))
import RIO.RIO (RIO())

data Game gameS teamS unitS tileS = Game
    { gameState     :: !gameS
    , gameTeams     :: !(Vector (Team gameS teamS unitS tileS))
    , gameTiles     :: !(MGrid tileS)
    , gameBehaviors :: !(IntMap (Behavior gameS teamS unitS tileS Game))
    } 

type Behavior gameS teamS unitS tileS behaving = 
    behaving gameS teamS unitS tileS -> RIO (Game gameS teamS unitS tileS -> Game gameS teamS unitS tileS)

data Team gameS teamS unitS tileS = Team
    { teamState      :: !teamS
    , teamUnits      :: !(IntMap (Unit gameS teamS unitS tileS))
    , teamPlayers    :: !(Vector Player)
    , teamVision     :: !(MGrid Int)
    , teamPathing    :: !(MGrid Int)
    , teamBehaviors  :: !(IntMap (Behavior gameS teamS unitS tileS Team))
    , teamSpawnCount :: {-# UNPACK #-} !Int -- Incremented with each new unit
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
    , unitBehaviors :: !(IntMap (Behavior gameS teamS unitS tileS Unit))
    , unitValues    :: !(Unit gameS teamS unitS tileS -> [(Word8,Word8)])
    } 

data UID = UID {uid, teamID :: {-# UNPACK #-} !Int}

data Order 
    = Standby
    | Move    !Point
    | Assault !Point
    | Target  !UID
    | Hold    !Point
    | Patrol  !Point !Point 
    | Invoke {-# UNPACK #-} !Int !Point

----------------------------------------------------------------------------
--                       # DATA SENT OVER WIRE #                          --
----------------------------------------------------------------------------

-- Datagram sent to clients from server
data ClientDatagram gameS teamS unitS tileS
    = EntityDatagram  [Unit gameS teamS unitS tileS]
    | TerrainDatagram [Terrain]
    | LineFXDatagram  [LineFX]
    | SFXDatagram     [SFX]
    | TargetDatagram  [TargetFX]
    | MessageDatagram Bool String String

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

instance Binary Terrain

data SFX = SFX
    { sfxId :: {-# UNPACK #-} !Word16
    , sfxX  :: {-# UNPACK #-} !Word16
    , sfxY  :: {-# UNPACK #-} !Word16
    , sfxZ  :: {-# UNPACK #-} !Word16
    } deriving (Generic)

instance Binary SFX

data TargetFX = TargetFX
    { targetfxId   :: {-# UNPACK #-} !Word16
    , targetfxUID  :: !Int
    , targetfxteam :: !Word8
    } deriving (Generic)

instance Binary TargetFX

data LineFX = LineFX
    { linefxId :: {-# UNPACK #-} !Word16
    , linefxXA :: {-# UNPACK #-} !Word16
    , linefxYA :: {-# UNPACK #-} !Word16
    , linefxZA :: {-# UNPACK #-} !Word16
    , linefxXB :: {-# UNPACK #-} !Word16
    , linefxYB :: {-# UNPACK #-} !Word16
    , linefxZB :: {-# UNPACK #-} !Word16
    } deriving (Generic)

instance Binary LineFX

data ClientMessage 
    = ActorMessage  !Bool -- True = Add to commands, False = Replace commands
     {-# UNPACK #-} !Int -- Type Id
     {-# UNPACK #-} !Float -- X coord
                    !Float -- Y coord
                    ![Int] -- List of actors to give command to
    | Invalid

instance Binary ClientMessage where
    get = getWord8 >>= \t -> case t of
        _ -> ActorMessage <$> get <*> get <*> get <*> get <*> get
    put = undefined