module WorldView exposing (viewWorld)

import Html exposing (Html)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Array exposing (Array)

import Model exposing (Model, Cell)
import Update exposing (Msg(..))


viewWorld : Model -> Html Msg
viewWorld model =
    let
        w = model.worldWidth
            
        h = model.worldHeight
            
        pxSize = model.pixelSize
                 
        svgLen len =
            toString <| len * pxSize

        ticks len =
            List.map (\x -> x * pxSize) [0 .. len]
    in
        svg
          [ width (svgLen w)
          , height (svgLen h)
          ]
          (List.concat
              [ viewPopulation model.world (pixel pxSize) 
              , (List.map (xgridLine (svgLen h)) <| ticks w)
              , (List.map (ygridLine (svgLen w)) <| ticks h)
              ]
          )


viewPopulation : Array Cell -> (Cell -> Svg Msg) -> List (Svg Msg)
viewPopulation world fn =
    Array.toList <| Array.map fn world


pixel : Int -> Cell -> Svg Msg
pixel pxSize cell =
    let
        (population, xcord, ycord) = cell

        size =
            toString pxSize
                
        posx =
            toString <| xcord * pxSize
                
        posy =
            toString <| ycord * pxSize
               
        popColor =
            if population == 1 then "black" else "white"
    in
        rect
          [ x posx
          , y posy
          , width size
          , height size
          , fill popColor
          , onClick <| GodFinger (xcord, ycord)
          ] []
            
            
xgridLine : String -> Int -> Svg Msg
xgridLine ymax xtick =
    line
      [ x1 (toString xtick)
      , x2 (toString xtick)
      , y1 "0"
      , y2 ymax
      , stroke "grey"
      ] []


          
ygridLine : String -> Int -> Svg Msg
ygridLine xmax ytick =
    line
      [ x1 "0"
      , x2 xmax
      , y1 (toString ytick)
      , y2 (toString ytick)
      , stroke "grey"] []


