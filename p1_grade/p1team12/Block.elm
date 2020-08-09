module Block exposing (..)

import Debug exposing (toString)
import Random
import Svg exposing (Svg, image, rect)
import Svg.Attributes exposing ( height, width, x, xlinkHref, y)
import String
import List exposing (..)

-- Block definition
type alias Block =
    { textureList : List String
    , eachScore : Int
    , xPos : Float
    , yPos : Float
    , exist : Bool
    , special : Special
    }

--The Special function of the block
type Special
    = Longer
    | Shorter
    | Lifes
    | Skills
    | NoSpecial

--Add special blocks to make the game more interesting
addSpecial : Random.Seed -> List Block -> List Block
addSpecial seed blocks =
    let
        total = (List.length blocks) // 10
        intGen = Random.list total (Random.int 0 ((List.length blocks) - 1))
        intList = Tuple.first (Random.step intGen seed)
        intToSpecial : Int -> Block -> (Special, List String)
        intToSpecial int block =
            case (modBy 4 int) of
                0 ->
                    case List.length block.textureList of
                        1 ->
                            (Longer, ["./assets/long3.png"])
                        2 ->
                            (Longer, ["./assets/long3.png", "./assets/long2.png"])
                        3 ->
                            (Longer, ["./assets/long3.png", "./assets/long2.png", "./assets/long1.png"])
                        _ ->
                            (Longer, [""])
                1 ->
                    case List.length block.textureList of
                        1 ->
                            (Shorter, ["./assets/short3.png"])
                        2 ->
                            (Shorter, ["./assets/short3.png", "./assets/short2.png"])
                        3 ->
                            (Shorter, ["./assets/short3.png", "./assets/short2.png", "./assets/short1.png"])
                        _ ->
                            (Shorter, [""])
                2 ->
                    case List.length block.textureList of
                        1 ->
                            (Lifes, ["./assets/life3.png"])
                        2 ->
                            (Lifes, ["./assets/life3.png", "./assets/life2.png"])
                        3 ->
                            (Lifes, ["./assets/life3.png", "./assets/life2.png", "./assets/life1.png"])
                        _ ->
                            (Lifes, [""])
                3 ->
                    case List.length block.textureList of
                        1 ->
                            (Skills, ["./assets/skill3.png"])
                        2 ->
                            (Skills, ["./assets/skill3.png", "./assets/skill2.png"])
                        3 ->
                            (Skills, ["./assets/skill3.png", "./assets/skill2.png", "./assets/skill1.png"])
                        _ ->
                            (Skills, [""])
                _ ->
                    (NoSpecial, [""])

        updateSpecial : Int -> Block -> Block
        updateSpecial int block =
            if List.member int intList then
                { block | special = Tuple.first (intToSpecial int block), textureList = Tuple.second (intToSpecial int block) }
            else
                block
    in
        List.indexedMap updateSpecial blocks

--Create different stages (1-6)
initBlock : Int -> List Block
initBlock stage=
    let
        countPosRow : Float -> Float -> Int -> Float
        countPosRow interval startX index =
            startX + (toFloat index) * (5 + interval)

        countPosCol : Float -> Float -> Int -> Float
        countPosCol interval startY index =
            startY + (toFloat index) * (2 + interval)

        initRow : List String -> Int -> Float -> Float -> Float -> Int -> Bool -> List Block
        initRow colorList eachScore yPos startX interval num exist =
            List.map (\x -> Block colorList eachScore x yPos exist NoSpecial) (List.map (countPosRow interval startX) (List.range 0 (num - 1)))

        initColumn : List String -> Int -> Float -> Float -> Float -> Int -> Bool -> List Block
        initColumn colorList eachScore xPos startY interval num exist =
            List.map (\y -> Block colorList eachScore xPos y exist NoSpecial) (List.map (countPosCol interval startY) (List.range 0 (num - 1)))

        durability3Color = [ "./assets/Wall3.png", "./assets/Wall2.png", "./assets/Wall1.png" ]
        durability2Color = [ "./assets/Wall3.png", "./assets/Wall2.png" ]
        durability1Color = [ "./assets/Wall3.png" ]

    in
        case stage of
            1 ->
                initRow durability2Color 8 2 2.5 0 14 True
                ++ initRow durability2Color 8 5 2.5 0 14 True
                ++ initRow durability1Color 4 8 2.5 0 14 True
                ++ initRow durability1Color 4 11 2.5 0 14 True
            2 ->
                initRow durability3Color 12 2 17.5 0 8 True
                ++ initRow durability2Color 8 5 2.5 0 14 True
                ++ initRow durability1Color 4 8 2.5 0 14 True
                ++ initRow durability3Color 12 11 2.5 0 5 True
                ++ initRow durability3Color 12 11 47.5 0 5 True
                ++ initRow durability2Color 8 14 17.5 0 8 True
                ++ initRow durability1Color 4 17 2.5 0 5 True
                ++ initRow durability1Color 4 17 47.5 0 5 True
            3 ->
                initColumn durability3Color 12 2.5 2 0 5 True
                ++ initColumn durability1Color 4 12.5 4 0 2 True
                ++ initColumn durability3Color 12 22.5 2 0 5 True
                ++ initColumn durability2Color 8 32.5 4 0 3 True
                ++ initColumn durability2Color 8 37.5 4 0 3 True
                ++ initColumn durability3Color 12 47.5 2 0 5 True
                ++ initColumn durability1Color 4 57.5 4 0 2 True
                ++ initColumn durability3Color 12 67.5 2 0 5 True
                ++ initRow durability3Color 12 10 2.5 0 5 True
                ++ initRow durability3Color 12 10 47.5 0 5 True
                ++ initRow durability2Color 8 13 2.5 0 5 True
                ++ initRow durability2Color 8 13 47.5 0 5 True
                ++ initColumn durability2Color 8 27.5 14 0 4 True
                ++ initColumn durability2Color 8 42.5 14 0 4 True
                ++ initRow durability1Color 4 17 32.5 0 2 True
                ++ initRow durability2Color 8 19 32.5 0 2 True
                ++ initColumn durability2Color 8 2.5 16 0 10 True
                ++ initColumn durability2Color 8 67.5 16 0 10 True
                ++ initRow durability2Color 8 34 7.5 0 5 True
                ++ initRow durability2Color 8 34 42.5 0 5 True
                ++ initColumn durability2Color 8 27.5 26 0 4 True
                ++ initColumn durability2Color 8 42.5 26 0 4 True
            4 ->
                initColumn durability3Color 12 25 1 0 18 True
                ++ initRow durability3Color 12 29 30 0 9 True
                ++ initRow durability3Color 12 35 0 0 5 True
                ++ initRow durability2Color 8 15 35 0 7 True
                ++ initRow durability2Color 8 13 40 0 5 True
                ++ initRow durability2Color 8 11 45 0 3 True
                ++ initRow durability2Color 8 9 50 0 1 True
                ++ initRow durability2Color 8 17 40 0 5 True
                ++ initRow durability2Color 8 19 45 0 3 True
                ++ initRow durability2Color 8 21 50 0 1 True
                ++ initRow durability1Color 4 1 0 0 5 True
                ++ initRow durability1Color 4 3 0 0 4 True
                ++ initRow durability1Color 4 5 0 0 3 True
                ++ initRow durability1Color 4 7 0 0 2 True
                ++ initRow durability1Color 4 9 0 0 1 True
                ++ initRow durability1Color 4 33 5 0 4 True
                ++ initRow durability1Color 4 31 10 0 3 True
                ++ initRow durability1Color 4 29 15 0 2 True
                ++ initRow durability1Color 4 27 20 0 1 True
            5 ->
                initRow durability1Color 4 1 0 0 15 True
                ++ initColumn durability3Color 12 5 7 0 14 True
                ++ initRow durability3Color 12 33 10 0 13 True
                ++ initRow durability1Color 4 7 10 0 1 True
                ++ initColumn durability2Color 8 20 3 0 14 True
                ++ initColumn durability3Color 12 35 7 0 13 True
                ++ initColumn durability3Color 12 40 3 0 15 True
                ++ initColumn durability2Color 8 60 3 0 15 True
                ++ initRow durability3Color 12 17 45 0 3 True
                ++ initRow durability3Color 12 17 65 0 2 True
            6 ->
                initRow durability2Color 8 1 0 0 15 True
                ++ initRow durability2Color 8 3 10 0 5 True
                ++ initRow durability2Color 8 3 40 0 5 True
                ++ initRow durability2Color 8 5 10 0 4 True
                ++ initRow durability2Color 8 5 45 0 4 True
                ++ initRow durability2Color 8 7 10 0 3 True
                ++ initRow durability2Color 8 7 50 0 3 True
                ++ initRow durability2Color 8 9 10 0 2 True
                ++ initRow durability2Color 8 9 55 0 2 True
                ++ initRow durability2Color 8 11 10 0 1 True
                ++ initRow durability2Color 8 11 60 0 1 True
                ++ initRow durability2Color 8 13 10 0 2 True
                ++ initRow durability2Color 8 13 55 0 2 True
                ++ initRow durability2Color 8 15 10 0 3 True
                ++ initRow durability2Color 8 15 50 0 3 True
                ++ initRow durability2Color 8 17 10 0 4 True
                ++ initRow durability2Color 8 17 45 0 4 True
                ++ initRow durability2Color 8 19 10 0 11 True
                ++ initRow durability3Color 12 21 30 0 3 True
                ++ initRow durability3Color 12 23 35 0 1 True
            _ ->
                initRow durability3Color 12 23 35 0 1 True

--Using Maybe-Just to get the head of model.blocks
currBlock : List Block-> Block
currBlock blocks =
    case List.head blocks of
      Just current ->
        current
      Nothing ->
        Block [ "#B51515", "#EB3F3F", "#E26A6A" ] 8 8 8 False NoSpecial

--Show blocks to users
svgBlocks : List Block -> List (Svg msg)
svgBlocks blocks =
    let
        svgBlock : Block -> Svg msg
        svgBlock block =
           if block.exist == True then
                image
                    [ x (toString block.xPos)
                    , y (toString block.yPos)
                    , xlinkHref (Maybe.withDefault "" (List.head block.textureList))
                    , width "5"
                    , height "2"
                    ]
                    []
           else
                rect [] []

    in
        List.map svgBlock blocks

--Collision detection module (blockCrash xDistance)
minimalPointX: Float -> Float -> Float -> Float
minimalPointX cx rx width =
    if (cx <= rx) then
        rx
    else if (cx >= rx + width) then
        rx + 5
    else
        cx

--Collision detection module (blockCrash yDistance)
minimalPointY: Float -> Float -> Float -> Float
minimalPointY cy ry height =
    if (cy <= ry) then
        ry
    else if (cy >= ry + height) then
        ry + 2
    else
        cy

--Collision detection module (blockCrash totalDistance)
distance: (Float, Float) -> (Float, Float) -> Float
distance (x1, y1) (x2, y2)=
    sqrt( (x1 - x2)^2 + (y1 - y2)^2 )
