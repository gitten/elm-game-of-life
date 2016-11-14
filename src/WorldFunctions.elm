module WorldFunctions exposing (..) --(Cord, newWorld, godFinger, handOfGod)

import Array exposing (Array)
import Random exposing (Generator)
import List.Extra as List

import Model exposing (Cell, World)



type alias Cord = (Int, Int)
type alias Index = Int


protoWorld : Int -> Int -> List Cord
protoWorld width height =
    let xs =
            [0 .. width - 1]
                
        ys =
            [0 .. height - 1]
    in
        List.andThen ys
        <| \y -> List.andThen xs
        <| \x -> [(x, y)]


mapPopulation :  List Int -> List Cord -> List Cell
mapPopulation population cords =
    List.map2 (\p (x,y) -> (p, x, y)) population cords


randomPopulation : Int -> Int -> Generator (List Int)
randomPopulation width height =
    Random.list (width * height) <| Random.int 0 1


worldBuilder : Int -> Int -> List Int ->  World
worldBuilder width height population =
        Array.fromList
            <| mapPopulation population
            <| protoWorld width height 


newWorld : Int -> Int -> World
newWorld width height =
    worldBuilder width height
    <| List.repeat (width * height) 0


blinkWorld : World
blinkWorld =
    let
        population =
            [ 0, 0, 0
            , 1, 1, 1
            , 0, 0, 0
            ]
    in
        worldBuilder 3 3 population
    

popToggle : Maybe Cell -> Cell
popToggle cell =
    case cell of
        Just (0, x, y) -> (1, x, y)
        Just (1, x, y) -> (0, x, y)
        _ ->
            (0, 0, 0)


cord2index : Int -> Cord -> Index
cord2index width cord =
    (fst cord) + (width * snd cord)


godFinger : World -> Int-> Cord -> World
godFinger world width cord =
    let
        index =
            cord2index width cord
                
        cell =
            Array.get index world
    in
        Array.set index (popToggle cell) world
    

getCellPopulation : World -> Index -> Int
getCellPopulation world index =
    case Array.get index world of
        Just (pop, _, _) -> pop
        _ -> 0


getNayborPop : World -> Int -> Cord -> Int
getNayborPop world width validCord =
    (getCellPopulation world) << (cord2index width) <| validCord


isValidCord : Int -> Int -> Cord -> Maybe Cord
isValidCord  width height cord =
    let
        (x, y) = cord
    in
        if (x < width) && (y < height) && (x >= 0) && (y >= 0)
        then Just cord    
        else Nothing


nayborCords : Int -> Cord -> List Cord
nayborCords width cord =
    let
        (x, y) = cord
        xTransform = [-1, 0, 1, -1, 1, -1, 0, 1]
        yTransform = [-1, -1, -1, 0, 0, 1, 1, 1]
        fn =
            \xt yt -> ((x+xt), (y+yt))
    in
        List.map2 fn xTransform yTransform


theReckoning : World -> Int -> Int -> Cord -> Int
theReckoning world width height cord =
    List.sum
        <| List.map (getNayborPop world width)
        <| List.filterMap (isValidCord width height)
        <| nayborCords width cord


judgement : Cell -> Int -> Cell
judgement cell nayborPopulation =
    let
        (cellPop, x, y) = cell
    in
        if  (&&) (cellPop == 1)
                 ((2 == nayborPopulation) || 3 == nayborPopulation)
            || ((nayborPopulation == 3) && (cellPop == 0))
        then (1, x, y)
        else (0, x, y)


handOfGod : World -> Int -> Int -> Cell -> Cell
handOfGod world width height cell =
    let
        (_, x, y) = cell
    in
        (judgement cell) << (theReckoning world width height) <| (x, y)
