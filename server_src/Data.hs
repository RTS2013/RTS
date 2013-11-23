{-# LANGUAGE MultiParamTypeClasses #-}

module Data where

import qualified Data.List as L
import qualified Data.Vector.Unboxed as U (Vector)
import qualified Local.Matrices.UnboxedMatrix2D as U (Matrix)
import Control.Applicative ((<$>),(<*>))
import Data.Word (Word8,Word16)
import Data.Vector (Vector)
import Data.Binary (Binary,Get,get,put)
import Data.Sequence (Seq)
import Data.Map.Strict (Map)
import Data.IntMap.Strict (IntMap)
import Data.ByteString.Lazy (ByteString)
import System.Random (StdGen)
import Network.Socket (Socket)
import Local.KDT (KDT)
import Local.Pathing (Move2D(..))

instance Eq Actor where
    a == b = identity_id (actor_identity a) == identity_id (actor_identity b)

instance Eq Team where
    a == b = team_id a == team_id b

toEntity :: (Int,Int,Int) -> Actor -> (Actor -> [(Word8,Word8)]) -> Entity
toEntity (mx,my,mz) a f = let
    ident = actor_identity a
    state = actor_state a
    moveS = actor_moveState a 
    normalizeCoord x = floor $ fromIntegral (maxBound :: Word16) / x
    normalizeAngle a = floor $ (255 / (pi*2)) * (if a < 0 then pi*2 - a else a) in
    Entity { entity_id = identity_id ident
           , entity_anim = actorState_animId state
           , entity_team = fromIntegral $ identity_team ident
           , entity_x = normalizeCoord $ moveState_x moveS
           , entity_y = normalizeCoord $ moveState_y moveS
           , entity_z = normalizeCoord $ moveState_z moveS
           , entity_angle = normalizeAngle $ moveState_angle moveS
           , entity_values = f a 
           } 

data World = World
    { world_teams :: !(Vector Team)
    , world_kdt   :: !(KDT Float Actor)
    , world_graph :: !(U.Matrix Word8)
    } 

data Team = Team 
    { team_id       :: {-# UNPACK #-} !Int
    , team_name     :: !String
    , team_entities :: !(Map Int Actor)
    , team_vision   :: !(U.Matrix Word16)
    , team_players  :: ![Player]
    } 

data Player = Player
    { player_team    :: {-# UNPACK #-} !Int
    , player_name    :: !String
    , player_secret  :: !String
    , player_status  :: !PlayerStatus
    , player_udpSock :: !Socket
    , player_tcpSock :: !Socket
    } 

data Actor = Actor
    { actor_identity  :: !Identity
    , actor_moveState :: !MoveState
    , actor_state     :: !ActorState
    } 

instance Move2D Float Actor where
    getWeight a = identity_weight $ actor_identity a
    getRadius a = identity_radius $ actor_identity a
    getX a = moveState_x $ actor_moveState a
    getY a = moveState_y $ actor_moveState a
    setX x a = a {actor_moveState = (actor_moveState a) {moveState_x = x}}
    setY y a = a {actor_moveState = (actor_moveState a) {moveState_y = y}}

-- An actors identity. What defines it as an actor.
data Identity = Identity
    { identity_id           :: {-# UNPACK #-} !Int 
    , identity_type         :: {-# UNPACK #-} !Int 
    , identity_team         :: {-# UNPACK #-} !Int 
    , identity_radius       :: {-# UNPACK #-} !Float 
    , identity_weight       :: {-# UNPACK #-} !Float 
    , identity_turnRate     :: {-# UNPACK #-} !Float 
    , identity_speedMax     :: {-# UNPACK #-} !Float 
    , identity_acceleration :: {-# UNPACK #-} !Float 
    , identity_selfEffects  :: !(IntMap (World -> Actor -> Maybe Actor))
    , identity_effects      :: !(IntMap (World -> Actor -> [Effect]))
    } 

-- An actors current position and orientation in the world
data MoveState = MoveState
    { moveState_x     :: {-# UNPACK #-} !Float
    , moveState_y     :: {-# UNPACK #-} !Float
    , moveState_z     :: {-# UNPACK #-} !Float
    , moveState_angle :: {-# UNPACK #-} !Float
    , moveState_speed :: {-# UNPACK #-} !Float
    } 

-- An actors current state
data ActorState = ActorState
    { actorState_animId :: {-# UNPACK #-} !Word8
    , actorState_random :: !StdGen           -- Every actor gets its own random generator
    , actorState_orders :: !(Seq Order)      -- Every actor has a sequence of orders to perform
    , actorState_values :: !(U.Vector Float) -- Every actor has arbitrary float values
    , actorState_flags  :: !(U.Vector Bool)  -- Every actor has arbitrary bool values
    , actorState_counts :: !(U.Vector Int)   -- Every actor has arbitrary int values
    } 

data Effect
    = ActorEffect {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(Actor -> Actor)
    | TeamEffect {-# UNPACK #-} !Int !(Team -> Team)
    | WorldEffect !(World -> World)

data Order 
    = Standby
    | Move    {-# UNPACK #-} !Float 
              {-# UNPACK #-} !Float
    | Assault {-# UNPACK #-} !Float 
              {-# UNPACK #-} !Float
    | Target  {-# UNPACK #-} !Int
    | Hold    {-# UNPACK #-} !Float 
              {-# UNPACK #-} !Float
    | Patrol  {-# UNPACK #-} !Float
              {-# UNPACK #-} !Float
              {-# UNPACK #-} !Float
              {-# UNPACK #-} !Float

data PlayerStatus
    = Playing
    | Observing
    | Disconnected

data ViewStatus
    = Visible
    | Discovered
    | Undiscovered

data Node = Node
    { node_isPath  :: Bool
    , node_height  :: {-# UNPACK #-} !Word8
    , node_visual  :: {-# UNPACK #-} !Word8
    , node_corners :: U.Vector (Int,Int)
    } 

---------------------------------
-- DATA TO TRANSFER OVER WIRES --
---------------------------------
data SFX = SFX
    { sfx_id :: {-# UNPACK #-} !Word16
    , sfx_x  :: {-# UNPACK #-} !Word16
    , sfx_y  :: {-# UNPACK #-} !Word16
    , sfx_z  :: {-# UNPACK #-} !Word16
    }

instance Binary SFX where
    get = SFX <$> get <*> get <*> get <*> get
    put a = do
        put $ sfx_id a
        put $ sfx_x a
        put $ sfx_y a
        put $ sfx_z a

data LineFX = LineFX
    { linefx_id :: {-# UNPACK #-} !Word16
    , linefx_xa :: {-# UNPACK #-} !Word16
    , linefx_ya :: {-# UNPACK #-} !Word16
    , linefx_za :: {-# UNPACK #-} !Word16
    , linefx_xb :: {-# UNPACK #-} !Word16
    , linefx_yb :: {-# UNPACK #-} !Word16
    , linefx_zb :: {-# UNPACK #-} !Word16
    }

instance Binary LineFX where
    get = LineFX <$> get <*> get <*> get <*> get <*> get <*> get <*> get 
    put a = do
        put $ linefx_id a
        put $ linefx_xa a
        put $ linefx_ya a
        put $ linefx_za a
        put $ linefx_xb a
        put $ linefx_yb a
        put $ linefx_zb a

data Terrain = Terrain
    { terrain_id  :: {-# UNPACK #-} !Word16
    , terrain_x   :: {-# UNPACK #-} !Word16
    , terrain_y   :: {-# UNPACK #-} !Word16
    , terrain_nwz :: {-# UNPACK #-} !Word16
    , terrain_nez :: {-# UNPACK #-} !Word16
    , terrain_swz :: {-# UNPACK #-} !Word16
    , terrain_sez :: {-# UNPACK #-} !Word16
    }

instance Binary Terrain where
    get = Terrain <$> get <*> get <*> get <*> get <*> get <*> get <*> get 
    put a = do
        put $ terrain_id  a
        put $ terrain_x   a
        put $ terrain_y   a
        put $ terrain_nwz a
        put $ terrain_nez a
        put $ terrain_swz a
        put $ terrain_sez a

data Entity = Entity
    { entity_id     :: {-# UNPACK #-} !Int
    , entity_team   :: {-# UNPACK #-} !Word8
    , entity_anim   :: {-# UNPACK #-} !Word8
    , entity_angle  :: {-# UNPACK #-} !Word8
    , entity_x      :: {-# UNPACK #-} !Word16
    , entity_y      :: {-# UNPACK #-} !Word16
    , entity_z      :: {-# UNPACK #-} !Word16
    , entity_values :: ![(Word8,Word8)]
    } 

instance Binary Entity where
    get = Entity <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> 
        ((get :: Get Word8) >>= \c -> genericReplicateM c get)
    put a = do
        put $ entity_id    a
        put $ entity_team  a
        put $ entity_anim  a
        put $ entity_angle a
        put $ entity_x     a
        put $ entity_y     a
        put $ entity_z     a
        if null $ entity_values a 
        then put (255 :: Word8)
        else mapM_ put $ entity_values a

data ActorMessage = ActorMessage 
        {-# UNPACK #-} !Int -- Team Id
                       !Int -- Type Id
                       !Bool -- True = Add to commands, False = Replace commands
        {-# UNPACK #-} !Float -- X coord
        {-# UNPACK #-} !Float -- Y coord
                       ![Int] -- List of actors to give command to

instance Binary ActorMessage where
    get = ActorMessage <$> get <*> get <*> get <*> get <*> get <*> get
    put (ActorMessage teamId typeId shift x y xs) = do
        put teamId
        put typeId
        put shift
        put x
        put y
        put xs
                
data HelloMessage = HelloMessage Int String String

instance Binary HelloMessage where
    get = HelloMessage <$> get <*> get <*> get
    put (HelloMessage team name secret) = do
        put team
        put name
        put secret

genericReplicateM :: (Integral i, Monad m) => i -> m a -> m [a]
genericReplicateM 0 m = return []
genericReplicateM n m = do
    x <- m
    xs <- genericReplicateM (n-1) m
    return $ x:xs

data ClientDatagram 
    = EntityDatagram  [Entity]
    | LineFXDatagram  [LineFX]
    | SFXDatagram     [SFX]
    | TerrainDatagram [Terrain]

instance Binary ClientDatagram where
    get = (get :: Get Word8) >>= \t -> case t of
        0 -> EntityDatagram  <$> ((get :: Get Word8) >>= \c -> genericReplicateM c get) 
        1 -> LineFXDatagram  <$> ((get :: Get Word8) >>= \c -> genericReplicateM c get) 
        2 -> SFXDatagram     <$> ((get :: Get Word8) >>= \c -> genericReplicateM c get) 
        3 -> TerrainDatagram <$> ((get :: Get Word8) >>= \c -> genericReplicateM c get) 
    put (EntityDatagram  xs) = put (L.genericLength xs :: Word8) >> mapM_ put xs 
    put (LineFXDatagram  xs) = put (L.genericLength xs :: Word8) >> mapM_ put xs 
    put (SFXDatagram     xs) = put (L.genericLength xs :: Word8) >> mapM_ put xs 
    put (TerrainDatagram xs) = put (L.genericLength xs :: Word8) >> mapM_ put xs 