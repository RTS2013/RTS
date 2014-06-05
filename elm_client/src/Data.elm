module Data where

import Touch 
import Grid
import Window
import Dict

gridW = 128
gridH = 128
gridWH = (gridW,gridH)

type UnitDict = Dict.Dict (Int,Int) (Unit,Unit)

type Game = 
    { grid : Grid.Grid Int
    , camera : Camera
    , input : Input
    , mode : Mode
    , units : UnitDict
    }

initGame : Game
initGame = 
    { grid = Grid.make gridWH 0
    , camera = initCamera
    , input = initInput
    , mode = Free
    , units = Dict.empty
    }

type Unit = 
    { animID : Int
    , typeID : Int
    , posX   : Float
    , posY   : Float
    , posZ   : Float
    , facing : Float
    , time   : Float
    , values : Dict.Dict Int Int
    }

type UnitInfo =
    { unitID : Int
    , teamID : Int
    , animID : Int
    , typeID : Int
    , posX   : Float
    , posY   : Float
    , posZ   : Float
    , facing : Float
    , valueList : [{k:Int,v:Int}]
    }

updateUnitDict : Time -> UnitInfo -> UnitDict -> UnitDict
updateUnitDict time ui ud = 
    let key = (ui.unitID,ui.teamID)
        maybePair = Dict.get key ud
        new = { animID = ui.animID
              , typeID = ui.typeID
              , posX   = ui.posX
              , posY   = ui.posY
              , posZ   = ui.posZ
              , facing = ui.facing
              , time   = time
              , values = Dict.fromList <| map (\kv -> (kv.k,kv.v)) ui.valueList
              } in
    case maybePair of
        Nothing -> Dict.insert key (new, new) ud
        Just ok -> Dict.insert key (fst ok, new) ud

type Camera =
    { posX     : Float
    , posY     : Float
    , camSpeed : Float
    , camAngle : Float
    } 

initCamera : Camera
initCamera =
    { posX = gridW / 2 * 64
    , posY = gridH / 2 * 64
    , camSpeed = 0
    , camAngle = 0
    } 

type Input =
    { left    : Bool
    , middle  : Bool
    , right   : Bool
    , mouse   : {posX:Float,posY:Float}
    , touches : [Touch.Touch]
    , unitData : {time:Float,unitInfos:[UnitInfo]}
    } 

initInput : Input
initInput =
    { left = False
    , middle = False
    , right = False
    , mouse = {posX=0,posY=0}
    , touches = []
    , unitData = {time=0,unitInfos=[]}
    }

data Mode
    = Free
    | MoveCamera
        { cx : Float -- Camera's X at drag start
        , cy : Float -- Camera's Y at drag start
        , mx : Float -- Mouse's X at drag start
        , my : Float -- Mouse's Y at drag start
        }