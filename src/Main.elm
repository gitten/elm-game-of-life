module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)

import Model exposing (..)
import Update exposing (Msg, update, ticker)
import WorldView exposing (viewWorld)
import UiView exposing (..)
import WorldFunctions exposing (newWorld)

main =
    App.program
        { init = init 30 25 20
        , view = view
        , update = update
        , subscriptions = ticker
        }
        

init : Int -> Int -> Int -> (Model, Cmd Msg)
init width height pxSize =
    let
        world =
            newWorld width height

        model' =
            model width height pxSize world
    in
        (model', Cmd.none)    


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ Html.text "Game of Life" ]
        , div []
            [ viewUi model
            , viewWorld model
            ]
        ]
