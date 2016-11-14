module Update exposing (Msg(..), PropKey(..), update, ticker)

import Array exposing (Array)
import Random exposing (Generator)
import Time exposing (Time, second)
import Task
import Cmd.Extra as Cmd

import Model exposing (Cell, World, Model)
import WorldFunctions exposing ( Cord, godFinger, handOfGod
                               , newWorld, randomPopulation
                               , worldBuilder
                               )



type PropKey
    = Width  (Int -> Int -> Int)
    | Height  (Int -> Int -> Int)
    | PixelSize (Int -> Int -> Int)


type Msg 
    = ChangeSize PropKey
    | GodFinger Cord
    | NextGeneration
    | Clear
    | Random
    | GenWorld (List Int)
    | SimState Bool
    | Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeSize key ->
                (updateSize model key, Cmd.message Clear)
               
        GodFinger cord ->
            let
                world =
                    updateCell model cord
                        
                model' =
                    { model | world = world }
            in
                (model', Cmd.none)

        NextGeneration ->
            let
                world =
                    updateWorld model

                model' =
                    { model | world = world }
            in
            (model', Cmd.none)

        Clear ->
            let
                world =
                    newWorld model.worldWidth model.worldHeight

                model' =
                    { model | world = world }
            in
                (model', Cmd.none)

        Random ->
            let
                population =
                    randomPopulation model.worldWidth model.worldHeight
            in
                (model, Random.generate GenWorld population)
                    
        GenWorld population ->
            let
                world =
                    worldBuilder model.worldWidth model.worldHeight population

                model' =
                    { model | world = world }
            in
                (model', Cmd.none)

        SimState state ->
            let
                model' =
                    { model | simState = state }
            in
                (model', Cmd.none)

        Tick _ ->
            let
                cmd = if model.simState
                      then Cmd.message NextGeneration
                      else Cmd.none
            in
                (model, cmd)



updateCell : Model -> Cord -> World
updateCell {worldWidth,world} cord =
    godFinger world worldWidth cord


updateWorld : Model -> World
updateWorld {worldWidth,worldHeight,world} =
        Array.map (handOfGod world worldWidth worldHeight) world


updateSize : Model -> PropKey -> Model 
updateSize model key =
    case key of
        Width fn ->
            { model | worldWidth = fn model.worldWidth 1 }

        Height fn ->
            { model | worldHeight = fn model.worldHeight 1 }     

        PixelSize fn ->
            { model | pixelSize = fn model.pixelSize 1 }

    
-- SUBSCRIPTIONS

ticker : Model -> Sub Msg
ticker model =
    Time.every (second / 7) Tick
