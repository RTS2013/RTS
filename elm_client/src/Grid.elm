module Grid where

import Array as A

data Grid a = Grid Int Int (A.Array a)

make : (Int,Int) -> a -> Grid a
make (w,h) a = Grid w h <| A.fromList <| repeat (w * h) a

set : (Int,Int) -> a -> Grid a -> Grid a
set (x,y) a (Grid w h arr) = Grid w h <| A.set (w * y + x) a arr

get : (Int,Int) -> Grid a -> a
get (x,y) (Grid w h arr) = A.getOrFail (w * y + x) arr

modify : (Int,Int) -> (a -> a) -> Grid a -> Grid a
modify xy f g = get xy g |> f |> \a -> set xy a g

slice : (Int,Int,Int,Int) -> Grid a -> [(Int,Int,a)]
slice (w,h,ox,oy) grid = concat <|
    flip map [ox - 1..ox + w - 1]  <| \x -> 
    flip map [oy - 1..oy + h - 1]  <| \y -> 
    (x, y, get (x,y) grid)

sliceBounds : (Int,Int,Int,Int) -> Grid a -> [(Int,Int,a)]
sliceBounds (w,h,ox,oy) grid = case grid of 
    Grid gw gh _ ->
        justs <| concat <|
        flip map [ox..ox+w]  <| \x -> 
        flip map [oy..oy+h]  <| \y -> 
        if x >= gw || x < 0 || y >= gh || y < 0 then 
            Nothing 
        else 
            Just (x, y, get (x,y) grid)