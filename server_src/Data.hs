module Data where

import qualified Data.Vector.Unboxed as U (Vector)
import qualified Local.Matrices.UnboxedMatrix2D as U (Matrix)
import Data.Word (Word8,Word16)
import Data.Vector (Vector)
import Data.Binary (Binary,Get,get,put)
import Data.Sequence (Seq)
import Data.Map.Strict (Map)
import Data.IntMap.Strict (IntMap)
import Data.ByteString.Lazy (ByteString)
import System.Random (StdGen)
import Network.Socket (Socket)
import Control.Monad (replicateM)
import Local.KDT (KDT)

instance Eq Actor where
    a == b = identity_id (actor_identity a) == identity_id (actor_identity b)

instance Eq Team where
    a == b = team_id a == team_id b

{-
toEntity :: Actor -> (Actor -> [(Word8,Word8)]) -> Entity
toEntity a f = let
    ident = actor_identity a
    state = actor_state a
    moveS = actor_moveState a 
    normalizeAngle a = max 0 $ min 255 $ floor $ (a + (pi/2)) / 255 in
    Entity { entity_id = identity_id ident
           , entity_team = fromIntegral $ identity_team ident
           , entity_x = moveState_x moveS
           , entity_y = moveState_y moveS
           , entity_z = moveState_z moveS
           , entity_angle = normalizeAngle $ moveState_angle moveS
           , entity_values = f a 
           }
-}

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

data Effect
    = ActorEffect {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(Actor -> Actor)
    | TeamEffect {-# UNPACK #-} !Int !(Team -> Team)
    | WorldEffect !(World -> World)

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
    , identity_selfEffects  :: !(IntMap (World -> Actor -> Actor))
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
    { actorState_random :: !StdGen           -- Every actor gets its own random generator
    , actorState_orders :: !(Seq Order)      -- Every actor has a sequence of orders to perform
    , actorState_values :: !(U.Vector Float) -- Every actor has arbitrary float values
    , actorState_flags  :: !(U.Vector Bool)  -- Every actor has arbitrary bool values
    , actorState_counts :: !(U.Vector Int)   -- Every actor has arbitrary int values
    } 

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
    | Quit

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
    get = do
        sfx_id <- get
        sfx_x <- get
        sfx_y <- get
        sfx_z <- get
        return $ SFX sfx_id sfx_x sfx_y sfx_z
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
    get = do
        linefx_id <- get
        linefx_xa <- get
        linefx_ya <- get
        linefx_za <- get
        linefx_xb <- get
        linefx_yb <- get
        linefx_zb <- get
        return $ LineFX linefx_id linefx_xa linefx_ya linefx_za linefx_xb linefx_yb linefx_zb
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
    get = do
        terrain_id  <- get
        terrain_x   <- get
        terrain_y   <- get
        terrain_nwz <- get
        terrain_nez <- get
        terrain_swz <- get
        terrain_sez <- get
        return $ Terrain terrain_id terrain_x terrain_y terrain_nwz terrain_nez terrain_swz terrain_sez
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
    , entity_angle  :: {-# UNPACK #-} !Word8
    , entity_x      :: {-# UNPACK #-} !Word16
    , entity_y      :: {-# UNPACK #-} !Word16
    , entity_z      :: {-# UNPACK #-} !Word16
    , entity_values :: ![(Word8,Word8)]
    } 

instance Binary Entity where
    get = do
        entity_id     <- get
        entity_angle  <- get
        entity_team   <- get
        entity_x      <- get
        entity_y      <- get
        entity_z      <- get
        count         <- get :: Get Word8
        entity_values <- replicateM (fromIntegral count) get
        return $ Entity entity_id entity_team entity_x entity_y entity_z entity_angle entity_values
    put a = do
        put $ entity_id    a
        put $ entity_team  a
        put $ entity_angle a
        put $ entity_x     a
        put $ entity_y     a
        put $ entity_z     a
        if null $ entity_values a 
        then put (255 :: Word8)
        else mapM_ put $ entity_values a

data ClientMessage 
    = MoveMessage 
        {-# UNPACK #-} !Int -- Team Id
                       !Bool -- True = Add to commands, False = Replace commands
        {-# UNPACK #-} !Float -- X coord
        {-# UNPACK #-} !Float -- Y coord
                       ![Int] -- List of actors to give command to

instance Binary ClientMessage where
    put (MoveMessage team_id shift x y xs) = do
        put team_id
        put shift
        put x
        put y
        put xs
    get = do
        t <- get :: Get Word8
        case t of
            0 -> do
                pid <- get
                b <- get
                x <- get
                y <- get
                ids <- get
                return $ MoveMessage pid b x y ids
                
data HelloMessage = HelloMessage Int String String

instance Binary HelloMessage where
    get = do
        team   <- get
        name   <- get
        secret <- get
        return $ HelloMessage team name secret
    put (HelloMessage team name secret) = do
        put team
        put name
        put secret