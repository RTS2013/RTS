module Data where

import Data.Int (Int64)
import Data.Word (Word8,Word16)
import Local.KDT (KDT)
import Data.Vector (Vector)
import Local.MatricesIO (Matrix2D)
import Data.Binary (Binary,get,put)

{-
This is where we will put all of the data for the game. Having all of the data
in one place allows you to really understand the structure of the program much
better than if it's scattered in multiple files. You'll be surprised at how much
info you can fit into such a small space. Also having it all in this file makes
it very easy to reference. import qualified Data as D
-}

data Actor = Actor
	{ actor_id :: !Int64
	, actor_team :: !Word8
	, actor_x :: !Word16
	, actor_y :: !Word16
	, actor_z :: !Word16
	, actor_facing :: !Word8
	, actor_radius :: !Word8
	, actor_weight :: !Float
	}

data Team = Team 
	{ team_id :: !Word8
	, team_entities :: !(Vector Actor)
	, team_vision :: !(Matrix2D Word16)
	}

data Player = Player
	{ player_id :: !Word8
	, player_name :: String
	, player_address :: String
	}

data World = World
	{ world_teams :: !(Vector Team)
	, world_kdt :: !(KDT Float Actor)
	, world_graph :: !(Vector (Vector Word16))
	}

data SFX = SFX
	{ sfx_id :: !Word16
	, sfx_x :: !Word16
	, sfx_y :: !Word16
	, sfx_z :: !Word16
	}

data LineFX = LineFX
	{ linefx_id :: !Word16
	, linefx_xa :: !Word16
	, linefx_ya :: !Word16
	, linefx_za :: !Word16
	, linefx_xb :: !Word16
	, linefx_yb :: !Word16
	, linefx_zb :: !Word16
	}

data Terrain = Terrain
	{ terrain_id :: !Word16
	, terrain_x :: !Word16
	, terrain_y :: !Word16
	, terrain_nwz :: !Word16
	, terrain_nez :: !Word16
	, terrain_swz :: !Word16
	, terrain_sez :: !Word16
	}

data Entity = Entity
	{ entity_id :: !Int64
	, entity_team :: !Word8
	, entity_x :: !Word16
	, entity_y :: !Word16
	, entity_z :: !Word16
	, entity_facing :: !Word8
	}

instance Eq Actor where
	a == b = actor_id a == actor_id b

instance Eq Team where
	a == b = team_id a == team_id b

instance Binary SFX where
	put a = do
		put $ sfx_id a
		put $ sfx_x a
		put $ sfx_y a
		put $ sfx_z a
	get = do
		a_id <- get
		a_x <- get
		a_y <- get
		a_z <- get
		return $ SFX a_id a_x a_y a_z

instance Binary LineFX where
	put a = do
		put $ linefx_id a
		put $ linefx_xa a
		put $ linefx_ya a
		put $ linefx_za a
		put $ linefx_xb a
		put $ linefx_yb a
		put $ linefx_zb a
	get = do
		a_id <- get
		a_xa <- get
		a_ya <- get
		a_za <- get
		a_xb <- get
		a_yb <- get
		a_zb <- get
		return $ LineFX a_id a_xa a_ya a_za a_xb a_yb a_zb

instance Binary Terrain where
	put a = do
		put $ terrain_id a
		put $ terrain_x a
		put $ terrain_y a
		put $ terrain_nwz a
		put $ terrain_nez a
		put $ terrain_swz a
		put $ terrain_sez a
	get = do
		a_id <- get
		a_x <- get
		a_y <- get
		a_nwz <- get
		a_nez <- get
		a_swz <- get
		a_sez <- get
		return $ Terrain a_id a_x a_y a_nwz a_nez a_swz a_sez

instance Binary Entity where
	put a = do
		put $ entity_id a
		put $ entity_team a
		put $ entity_x a
		put $ entity_y a
		put $ entity_z a
		put $ entity_facing a
	get = do
		a_id <- get
		a_team <- get
		a_x <- get
		a_y <- get
		a_z <- get
		a_f <- get
		return $ Entity a_id a_team a_x a_y a_z a_f