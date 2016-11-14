module UiView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Update exposing (Msg(..), PropKey(..))



viewUi : Model -> Html Msg
viewUi model =
    div []
        [ h2 [] [ text "Click on all the things!"]
        , changeSize model
        , simButton model
        , nextGenButton
        , randomButton
        , clearButton
        ]


simButton : Model -> Html Msg
simButton model =
    let
        state = model.simState
    in
        button
          [ onClick <| SimState <| not state ]
          [ text <| if state then "Pause?" else "Run?"]


randomButton : Html Msg
randomButton =
    button [ onClick Random ] [ text "Random Population"]


clearButton : Html Msg
clearButton =
    button [ onClick Clear ] [ text "Clear World" ]


nextGenButton : Html Msg
nextGenButton =
    button [ onClick NextGeneration ] [ text "Next Generation"]
        

changeSize : Model -> Html Msg
changeSize model =
    div []
        [ text "Width : "
        , button [ onClick <| ChangeSize <| Width (-) ] [ text "-" ]
        , text <| toString  model.worldWidth
        , button [ onClick <| ChangeSize <| Width (+) ] [ text "+" ]
                    
        , text "Height : "
        , button [ onClick <| ChangeSize <| Height (-) ] [ text "-" ]
        , text <| toString model.worldHeight
        , button [ onClick <| ChangeSize <| Height (+) ] [ text "+" ]
                    
        , text "Pixel Size : "
        , button [ onClick <| ChangeSize <| PixelSize (-) ] [ text "-" ]
        , text <| toString model.pixelSize
        , button [ onClick <| ChangeSize <| PixelSize (+) ] [ text "+" ]
        ]
        
