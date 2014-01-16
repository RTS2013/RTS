{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data where

import Data.List (genericLength)
import Control.Applicative ((<$>),(<*>))
import Data.Binary (Binary,get,put,getWord8,putWord8)
import Data.Vector (Vector)
import Data.Text.Binary ()
import Data.Word (Word8,Word16)
import Data.IntMap.Strict (IntMap)
import Data.Sequence (Seq)
import Grid.UnboxedGrid (Grid)
import System.Random (StdGen)
import GoodTimes (Player)
import Movement (Point(..),Bulk)

data GridEdit tileS = GridEdit {-# UNPACK #-} !Int -- Grid X
                               {-# UNPACK #-} !Int -- Grid Y
                                              !tileS

data ViewEdit = ViewIncr {-# UNPACK #-} !Int -- Team
                         {-# UNPACK #-} !Int -- Grid X
                         {-# UNPACK #-} !Int -- Grid Y
              | ViewDecr {-# UNPACK #-} !Int -- Team
                         {-# UNPACK #-} !Int -- Grid X
                         {-# UNPACK #-} !Int -- Grid Y

data Game gameS teamS unitS tileS = Game
    { gameState :: !gameS
    , gameTeams :: !(Vector (Team gameS teamS unitS tileS))
    , gameTiles :: !(Grid tileS)
    } 

type Behavior gameS teamS unitS tileS behaving = 
    behaving gameS teamS unitS tileS -> 
        Game gameS teamS unitS tileS -> 
            ( Game gameS teamS unitS tileS
            , [ViewEdit]
            , [GridEdit tileS]
            )

data Team gameS teamS unitS tileS = Team
    { teamState      :: !teamS
    , teamUnits      :: !(IntMap (Unit gameS teamS unitS tileS))
    , teamPlayers    :: !(Vector Player)
    , teamVision     :: !(Grid Int)
    , teamSpawnCount :: {-# UNPACK #-} !Int -- Incremented with each new unit
    } 

data Unit gameS teamS unitS tileS = Unit
    { unitIdentity :: !(UnitIdentity gameS teamS unitS tileS)
    , unitPoint    :: !Point
    , unitBulk     :: !Bulk
    , unitOrders   :: !(Seq Order)
    , unitRandom   :: !StdGen
    , unitState    :: !unitS
    } 

data UnitIdentity gameS teamS unitS tileS = UnitIdentity
    { unitUID       :: !UID
    , unitType      :: {-# UNPACK #-} !Int 
    , unitBehaviors :: !(IntMap (Behavior gameS teamS unitS tileS Unit))
    , unitValues    :: !(Unit gameS teamS unitS tileS -> [(Word8,Word8)])
    } 

data UID = UID {uid, teamID :: {-# UNPACK #-} !Int}

data Order 
    = Standby
    | Move !Point
    | Assault !Point
    | Target !UID
    | Hold !Point
    | Patrol !Point !Point
    | Invoke {-# UNPACK #-} !Int !Point

----------------------------------------------------------------------------
--                       # DATA SENT OVER WIRE #                          --
----------------------------------------------------------------------------

-- Datagram sent to clients from server
data ClientDatagram 
    = EntityDatagram  [Entity]
    | TerrainDatagram [Terrain]
    | LineFXDatagram  [LineFX]
    | SFXDatagram     [SFX]
    | TargetDatagram  [TargetFX]
    | MessageDatagram Bool String String

{-
toEntity :: Actor -> (Actor -> [(Word8,Word8)]) -> Entity
toEntity a f = let 
    ident = identity a 
    state = attributes a 
    moveS = movement a 
    normalizeCoord x = floor $ fromIntegral (maxBound :: Word16) / x
    normalizeAngle a = floor $ (255 / (pi*2)) * (if a < 0 then pi*2 - a else a) in
    Entity { entityId = actorId ident
           , entityAnimation = animation state
           , entityTeam = fromIntegral $ actorTeam ident
           , entityX = normalizeCoord $ actorX moveS
           , entityY = normalizeCoord $ actorY moveS
           , entityZ = normalizeCoord $ actorZ moveS
           , entityFacing = normalizeAngle $ actorFacing moveS
           , entityValues = f a 
           } 
-}

-- 14 Bytes Minimum
data Entity = Entity
    { entityId        :: {-# UNPACK #-} !Int
    , entityTeam      :: {-# UNPACK #-} !Word8
    , entityAnimation :: {-# UNPACK #-} !Word8
    , entityFacing    :: {-# UNPACK #-} !Word8
    , entityX         :: {-# UNPACK #-} !Word16
    , entityY         :: {-# UNPACK #-} !Word16
    , entityZ         :: {-# UNPACK #-} !Word16
    , entityValues    :: ![(Word8,Word8)]
    } 

instance Binary Entity where
    get = undefined
    put a = do
        put $ entityId        a
        put $ entityTeam      a
        put $ entityAnimation a
        put $ entityFacing    a
        put $ entityX         a
        put $ entityY         a
        put $ entityZ         a
        let xs = entityValues a
        putWord8 (genericLength xs) >> mapM_ put xs

-- 8 Bytes
data Terrain = Terrain
    { terrainId :: {-# UNPACK #-} !Word16
    , terrainX  :: {-# UNPACK #-} !Word16
    , terrainY  :: {-# UNPACK #-} !Word16
    , terrainZ  :: {-# UNPACK #-} !Word16
    } 

instance Binary Terrain where
    get = undefined
    put = undefined

data SFX = SFX
    { sfxId :: {-# UNPACK #-} !Word16
    , sfxX  :: {-# UNPACK #-} !Word16
    , sfxY  :: {-# UNPACK #-} !Word16
    , sfxZ  :: {-# UNPACK #-} !Word16
    } 

instance Binary SFX where
    get = undefined
    put a = do
        put $ sfxId a
        put $ sfxX a
        put $ sfxY a
        put $ sfxZ a

data TargetFX = TargetFX
    { targetfxId  :: {-# UNPACK #-} !Word16
    , targetfxUID :: !UID
    } 

instance Binary TargetFX where
    get = undefined
    put a = do
        put $ targetfxId a
        put $ uid $ targetfxUID a
        put $ teamID $ targetfxUID a

-- 14 Bytes
data LineFX = LineFX
    { linefxId :: {-# UNPACK #-} !Word16
    , linefxXA :: {-# UNPACK #-} !Word16
    , linefxYA :: {-# UNPACK #-} !Word16
    , linefxZA :: {-# UNPACK #-} !Word16
    , linefxXB :: {-# UNPACK #-} !Word16
    , linefxYB :: {-# UNPACK #-} !Word16
    , linefxZB :: {-# UNPACK #-} !Word16
    } 

instance Binary LineFX where
    get = undefined
    put a = do
        put $ linefxId a
        put $ linefxXA a
        put $ linefxYA a
        put $ linefxZA a
        put $ linefxXB a
        put $ linefxYB a
        put $ linefxZB a

data ClientMessage 
    = ActorMessage
                        !Bool -- True = Add to commands, False = Replace commands
        {-# UNPACK #-}  !Int -- Type Id
        {-# UNPACK #-}  !Point
                        ![Int] -- List of actors to give command to
    | Invalid

instance Binary ClientMessage where
    get = getWord8 >>= \t -> case t of
        _ -> ActorMessage <$> get <*> get <*> get <*> get
    put = undefined


instance Binary Point where
    get = Point <$> get <*> get <*> get
    put a = do
        put $ xCoord a
        put $ yCoord a
        put $ zCoord a