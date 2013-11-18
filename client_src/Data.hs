module Data where

import Data.Word (Word8,Word16)
import Data.Binary (Binary,get,put,Get)
import Control.Monad (replicateM)

data SFX = SFX
    { sfx_id :: !Word16
    , sfx_x :: !Word16
    , sfx_y :: !Word16
    , sfx_z :: !Word16
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
    { linefx_id :: !Word16
    , linefx_xa :: !Word16
    , linefx_ya :: !Word16
    , linefx_za :: !Word16
    , linefx_xb :: !Word16
    , linefx_yb :: !Word16
    , linefx_zb :: !Word16
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
    { terrain_id :: !Word16
    , terrain_x :: !Word16
    , terrain_y :: !Word16
    , terrain_nwz :: !Word16
    , terrain_nez :: !Word16
    , terrain_swz :: !Word16
    , terrain_sez :: !Word16
    }

instance Binary Terrain where
    get = do
        terrain_id <- get
        terrain_x <- get
        terrain_y <- get
        terrain_nwz <- get
        terrain_nez <- get
        terrain_swz <- get
        terrain_sez <- get
        return $ Terrain terrain_id terrain_x terrain_y terrain_nwz terrain_nez terrain_swz terrain_sez
    put a = do
        put $ terrain_id a
        put $ terrain_x a
        put $ terrain_y a
        put $ terrain_nwz a
        put $ terrain_nez a
        put $ terrain_swz a
        put $ terrain_sez a

data Entity = Entity
    { entity_id :: !Int
    , entity_team :: !Word8
    , entity_x :: !Float
    , entity_y :: !Float
    , entity_z :: !Float
    , entity_angle :: !Word8
    , entity_values :: ![(Word8,Word8)]
    }

instance Binary Entity where
    get = do
        entity_id <- get
        entity_team <- get
        entity_x <- get
        entity_y <- get
        entity_z <- get
        entity_angle <- get
        count <- get :: Get Word8
        entity_values <- replicateM (fromIntegral count) get
        return $ Entity entity_id entity_team entity_x entity_y entity_z entity_angle entity_values
    put a = do
        put $ entity_id a
        put $ entity_team a
        put $ entity_x a
        put $ entity_y a
        put $ entity_z a
        put $ entity_angle a
        if null $ entity_values a 
        then put (255 :: Word8)
        else mapM_ put $ entity_values a

data ClientMessage 
    = MoveMessage 
        Int -- Team Id
        Bool -- True = Add to commands, False = Replace commands
        Float -- X coord
        Float -- Y coord
        [Int] -- List of actors to give command to

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