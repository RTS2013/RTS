module Main where

import Data (..)
import Dict
import Grid
import Touch
import Window
import Transform2D as T

port mousePosition : Signal {posX:Float,posY:Float}
port leftDown      : Signal Bool
port rightDown     : Signal Bool
port middleDown    : Signal Bool
port unitData      : Signal 
    { time : Float
    , unitInfos : 
        [
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
        ]
    }

inputS : Signal Input
inputS = 
    (\left mid right pos touches unitData -> 
        { left = left
        , middle = mid
        , right = right
        , mouse = pos
        , touches = touches
        , unitData = unitData
        }) 
    <~ leftDown
    ~  middleDown
    ~  rightDown 
    ~  mousePosition
    ~  Touch.touches
    ~  unitData

updateGame : Input -> Game -> Game
updateGame i g = 
    case g.mode of
    Free -> 
        if i.middle then 
            let mode = MoveCamera { cx = g.camera.posX
                                  , cy = g.camera.posY
                                  , mx = i.mouse.posX
                                  , my = i.mouse.posY } in
            updateUnits i 
            { g 
            | input <- i
            , mode <- mode }
        else
            updateUnits i 
            { g 
            | input <- i }
    MoveCamera mc ->
        if i.middle then 
            let cam = g.camera
                cam' = { cam 
                       | posX <- mc.cx - (g.input.mouse.posX - mc.mx) 
                       , posY <- mc.cy + (g.input.mouse.posY - mc.my) } in
            updateUnits i 
            { g 
            | input <- i
            , camera <- cam' }
        else
            updateUnits i 
            { g 
            | input <- i
            , mode <- Free }

updateUnits : Input -> Game -> Game
updateUnits i g = 
    { g 
    | units <- foldl (updateUnitDict i.unitData.time) g.units i.unitData.unitInfos }

gameS : Signal Game
gameS = foldp updateGame initGame inputS

main : Signal Element
main = display <~ (sampleOn (fps 10) gameS) ~ Window.dimensions

cthulhu = toForm (image 64 64 "build/resources/pure_evil.png")

display : Game -> (Int,Int) -> Element
display g (w,h) =
    let screenW = toFloat w
        screenH = toFloat h
        cameraX = g.camera.posX
        cameraY = g.camera.posY
        slice = 
            flip Grid.sliceBounds g.grid
            ( ceiling <| screenW / 64 + 1
            , ceiling <| screenH / 64 + 1
            , floor   <| (cameraX - screenW / 2) / 64
            , floor   <| (cameraY - screenH / 2) / 64
            ) 
        tiles = 
            group <| map (\(mouseX,mouseY,a) -> square 65
            |> filled (rgb 255 255 255 )
            |> move ( toFloat mouseX * 64 - cameraX
                    , toFloat mouseY * 64 - cameraY
                    ) ) slice
        units = 
            map snd (Dict.values g.units)
            |> filter (\u -> (u.posX * 64 - cameraX)^2 + (u.posY * 64 - cameraY)^2 < (screenW/2)^2 + (screenH/2)^2)
            |> map (\u -> cthulhu |> move (u.posX * 64 - cameraX, u.posY * 64 - cameraY)) 
            |> group in
    collage w h [tiles,units]