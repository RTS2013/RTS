module Data where

import Data.Int (Int64)
import Data.Word (Word8,Word16,Word64)
import Local.KDT (KDT)
import Data.Vector (Vector)
import Local.Matrices.UnboxedMatrix2D (Matrix)
import Data.Binary (Binary,get,put,Get)
import Data.Sequence (Seq)
import Data.Map.Strict (Map)
import Data.IntMap.Strict (IntMap)
import Data.ByteString.Lazy (ByteString)

{-
This is where we will put all of the data for the game. Having all of the data
in one place allows you to really understand the structure of the program much
better than if it's scattered in multiple files. You'll be surprised at how much
info you can fit into such a small space. Also having it all in this file makes
it very easy to reference. import qualified Data as D
-}

instance Eq Actor where
	a == b = actor_id a == actor_id b

instance Eq Team where
	a == b = team_id a == team_id b

type TeamID = Int
type PlayerID = Int
type ActorID = Word64

data Actor = Actor
	{ actor_id :: !ActorID
	, actor_type :: !Word64
	, actor_team :: !TeamID
	, actor_movement :: !Movement
	, actor_orders :: !(Seq Order)
	, actor_values :: !(IntMap Float)
	, actor_flags :: !(IntMap Bool)
	, actor_counts :: !(IntMap Int)
	, actor_world_effects :: !(IntMap (World -> Actor -> World))
	, actor_team_effects :: !(IntMap (World -> Actor -> [(TeamID, Team -> Team)]))
	, actor_actor_effects :: !(IntMap (World -> Actor -> [(TeamID, ActorID, Actor -> Actor)]))
	, actor_self_effects :: !(IntMap (World -> Actor -> Actor))
	}

data Movement = Movement 
	{ coord_x :: !Float 
	, coord_y :: !Float 
	, coord_z :: !Float 
	, radius :: !Float 
	, weight :: !Float 
	, facing :: !Float 
	, turn_rate :: !Float 
	, speed_max :: !Float 
	, speed_now :: !Float 
	, acceleration :: !Float 
	}

data Order 
	= Standby -- Standing around doing nothing...
	| Move Float Float -- Ignore enemies on way to destination
	| Assault Float Float -- Attack enemies on way to destination
	| Attack ActorID -- Attack a specific enemy, ignoring all others
	| Assist ActorID -- Follow and assist unit
	| Hold Float Float -- Hold position and attack enemies in range
	| Patrol Float Float Float Float -- Patrol between point A and point B

data Team = Team 
	{ team_id :: !TeamID
	, team_name :: String
	, team_entities :: !(Map ActorID Actor)
	, team_vision :: !(Matrix Word16)
	}

data Player = Player
	{ player_id :: !PlayerID
	, player_name :: String
	}

data World = World
	{ world_teams :: !(Vector Team)
	, world_kdt :: !(KDT Float Actor)
	, world_graph :: !(Matrix Word16)
	}

data PlayerStatus
	= Playing
	| Observing
	| Quit

data ServerState = ServerState
	{ server_num_connections :: Int
	, server_client_messages :: [ClientMessage]
	}

---------------------------------
-- DATA TO TRANSFER OVER WIRES --
---------------------------------
data SFX = SFX
	{ sfx_id :: !Word16
	, sfx_x :: !Word16
	, sfx_y :: !Word16
	, sfx_z :: !Word16
	}

instance Binary SFX where
	get = undefined
	put a = do
		put $ sfx_id a
		put $ sfx_x a
		put $ sfx_y a
		put $ sfx_z a

data LineFX = LineFX
	{ linefx_id :: !Word16
	, linefx_xa :: !Word16
	, linefx_ya :: !Word16
	, linefx_za :: !Word16
	, linefx_xb :: !Word16
	, linefx_yb :: !Word16
	, linefx_zb :: !Word16
	}

instance Binary LineFX where
	get = undefined
	put a = do
		put $ linefx_id a
		put $ linefx_xa a
		put $ linefx_ya a
		put $ linefx_za a
		put $ linefx_xb a
		put $ linefx_yb a
		put $ linefx_zb a

data Terrain = Terrain
	{ terrain_id :: !Word16
	, terrain_x :: !Word16
	, terrain_y :: !Word16
	, terrain_nwz :: !Word16
	, terrain_nez :: !Word16
	, terrain_swz :: !Word16
	, terrain_sez :: !Word16
	}

instance Binary Terrain where
	get = undefined
	put a = do
		put $ terrain_id a
		put $ terrain_x a
		put $ terrain_y a
		put $ terrain_nwz a
		put $ terrain_nez a
		put $ terrain_swz a
		put $ terrain_sez a

data Entity = Entity
	{ entity_id :: !Int64
	, entity_team :: !Word8
	, entity_x :: !Word16
	, entity_y :: !Word16
	, entity_z :: !Word16
	, entity_facing :: !Word8
	, entity_values :: ![(Word8,Word8)]
	}

instance Binary Entity where
	get = undefined
	put a = do
		put $ entity_id a
		put $ entity_team a
		put $ entity_x a
		put $ entity_y a
		put $ entity_z a
		put $ entity_facing a
		put $ entity_values a
	
data ClientMessage 
	= MoveMessage 
		PlayerID
		Bool -- True = Add to commands, False = Replace commands
		Float -- X coord
		Float -- Y coord
		[ActorID] -- List of actors to give command to

instance Binary ClientMessage where
	put = undefined
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