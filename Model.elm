module Model exposing (..)

import Array exposing (Array)




type alias Cell = (Int, Int, Int)


type alias World = Array Cell    


type alias Model =
    { worldWidth : Int
    , worldHeight : Int
    , pixelSize : Int
    , simState : Bool
    , world : World
    }

model : Int -> Int -> Int -> World -> Model
model width height pxSize world =
    { worldWidth = width
    , worldHeight = height
    , pixelSize = pxSize
    , simState = False
    , world = world
    }



