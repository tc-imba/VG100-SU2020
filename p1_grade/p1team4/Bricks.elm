module Bricks exposing (..)

import Color exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random

type Brick_attribute
    = Pride -- Humility
    | Envy -- Kindness
    | Greed -- Charity
    | Wrath -- Patience

type Elf_attribute
    = Patience
    | Charity
    | Kindness
    | Humility

type alias Brick =
    { position : ( Int, Int )
    , width : Int
    , height : Int
    , ifExist : Bool
    , color : String
    , attribute : Brick_attribute
    , lives : Int
    }

abrick : Brick
abrick =
    { position = ( 0, 0 )
    , width = 70
    , height = 50
    , ifExist = True
    , color = "url(#base1)"
    , attribute = Pride
    , lives = 2
    }

positions =
    [(15,0),(105,0),(195,0),(285,0),(375,0),(465,0),(555,0),(645,0),(735,0),(825,0)
    ,(5,70),(95,70),(185,70),(275,70),(365,70),(455,70),(545,70),(635,70),(725,70),(815,70)
    ,(15,140),(105,140),(195,140),(285,140),(375,140),(465,140),(555,140),(645,140),(735,140),(825,140)
    ]

randomcolor a=
    if a == 1 then
        "url(#base1)"
    else if a == 2 then
        "url(#base2)"
    else if a == 3 then
        "url(#base3)"
    else if a == 4 then
        "url(#base4)"
    else
        "url(#base1)"

intList : Random.Generator (List Int)
intList =
    Random.list 50 (Random.int 1 4)

getcolorlist =
    Random.step intList (Random.initialSeed 12311)
        |> Tuple.first

pairs : List a -> List b -> List (a,b)
pairs xs ys =
  List.map2 Tuple.pair xs ys

getcolorlist_ =
    List.map
    (\a -> randomcolor a)
    getcolorlist

getcolor =
    pairs getcolorlist_ positions

color2Attribute : Brick -> Brick
color2Attribute abrick_ =
    let
        origin = abrick_
        att =
            case abrick_.color of
                "url(#base1)" -> Pride
                "url(#base2)" -> Envy
                "url(#base3)" -> Greed
                _ -> Wrath

    in
    { origin | attribute = att }



formlist : (String, (Int, Int)) -> Brick
formlist a =
    let
        color = Tuple.first a
        position = Tuple.second a
    in
        {abrick|color = color, position = position}

bricks : List Brick
bricks =
    List.map (\a -> formlist a ) getcolor
    |> List.map (\a -> color2Attribute a)

formsvg : Brick -> Svg msg
formsvg brick =
    case brick.ifExist of
        True ->
            rect
                [ x (Tuple.first(brick.position)|>Debug.toString)
                , y (Tuple.second(brick.position)|>Debug.toString)
                , width (brick.width|>Debug.toString)
                , height (brick.height|>Debug.toString)
                , stroke background
                , fill (brick.color)
                ]
                []
        False ->
            rect
                [ x (Tuple.first(brick.position)|>Debug.toString)
                , y (Tuple.second(brick.position)|>Debug.toString)
                , width (brick.width|>Debug.toString)
                , height (brick.height|>Debug.toString)
                , stroke background
                , fill background
                , opacity "0"
                ]
                []

tieformsvg_ : Brick -> Svg msg
tieformsvg_ brick =
        case brick.ifExist of
        True ->
            rect
                [ x ((Tuple.first(brick.position)+35)|>Debug.toString)
                , y ((Tuple.second(brick.position))|>Debug.toString)
                , width "10"
                , height (brick.height|>Debug.toString)
                , fill tie
                , opacity "0.8"
                , stroke background
                , Svg.Attributes.mask "url(#myMask)"
                ]
                []
        False ->
            rect
                [ x (Tuple.first(brick.position)|>Debug.toString)
                , y (Tuple.second(brick.position)|>Debug.toString)
                , width "90"
                , height "70"
                , fill background
                , opacity "0"
                ]
                []

topformsvg : Brick -> Svg msg
topformsvg brick =
        case brick.ifExist of
        True ->
            rect
                [ x ((Tuple.first(brick.position)+(-5))|>Debug.toString)
                , y ((Tuple.second(brick.position))|>Debug.toString)
                , width ((brick.width+10)|>Debug.toString)
                , height "20"
                , fill brick.color
                , stroke background
                ]
                []
        False ->
            rect
                [ x (Tuple.first(brick.position)|>Debug.toString)
                , y (Tuple.second(brick.position)|>Debug.toString)
                , width "90"
                , height "70"
                , fill background
                , opacity "0"
                ]
                []


tosvg : List Brick -> List (Svg msg)
tosvg brick_s=
    List.map (\a -> formsvg a) brick_s

tietosvg_ : List Brick -> List (Svg msg)
tietosvg_ brick_s=
    List.map (\a -> tieformsvg_ a) brick_s

toptosvg_ : List Brick -> List (Svg msg)
toptosvg_ brick_s=
    List.map (\a -> topformsvg a) brick_s

type alias Board =
    { x : Int
    , y : Float
    , width : Float
    , height : Float
    }

boardini =
    { x = 350
    , y = 470
    , width = 200
    , height = 8
    }



intToFloat : Int -> Float
intToFloat a =
    let
        c = Debug.toString a
        o = String.toFloat c
    in
    Maybe.withDefault 0 o
boardtosvg board =
    let
        x_ = board.x |> intToFloat
        y_ = board.y
        wid = board.width
        hei = board.height
    in
    [  ellipse
        [ cx (x_+1.15*wid|>Debug.toString)
        , cy (y_+0.5*hei|>Debug.toString)
        , rx (0.15*wid|>Debug.toString)
        , ry (2*hei|>Debug.toString)
        , fill "url(#ring)"]
        []
      , rect
        [ x (x_ |> Debug.toString)
        , y (y_ |> Debug.toString)
        , width (wid |> Debug.toString)
        , height (hei |> Debug.toString)
        , fill "url(#wand)"
        ]
        []
      , polygon
        [ points (
            (x_+wid|>Debug.toString) ++ "," ++ (y_|>Debug.toString) ++ " "
            ++ (x_+1.05*wid|>Debug.toString) ++ "," ++ (y_ - 0.3*hei|>Debug.toString) ++ " "
            ++ (x_+1.2*wid|>Debug.toString) ++ "," ++ (y_ - 0.3*hei|>Debug.toString) ++ " "
            ++ (x_+1.25*wid|>Debug.toString) ++ "," ++ (y_+0.2*hei|>Debug.toString) ++ " "
            ++ (x_+1.25*wid|>Debug.toString) ++ "," ++ (y_+0.8*hei|>Debug.toString) ++ " "
            ++ (x_+1.2*wid|>Debug.toString) ++ "," ++ (y_ + 1.3*hei|>Debug.toString) ++ " "
            ++ (x_+1.05*wid|>Debug.toString) ++ "," ++ (y_ + 1.3*hei|>Debug.toString) ++ " "
            ++ (x_+wid|>Debug.toString) ++ "," ++ (y_+hei|>Debug.toString)
            )
        , stroke color_crys_stroke
        , strokeWidth "1px"
        , fill "url(#crystal)"
        ]
        []
      , line
        [ x1 (x_+1.05*wid|>Debug.toString)
        , y1 (y_ - 0.3*hei|>Debug.toString)
        , x2 (x_+1.11*wid|>Debug.toString)
        , y2 (y_+0.5*hei|>Debug.toString)
        , stroke color_crys_stroke
        , strokeWidth "0.6px"
        ]
        []
      , line
        [ x1 (x_+1.05*wid|>Debug.toString)
        , y1 (y_ + 1.3*hei|>Debug.toString)
        , x2 (x_+1.11*wid|>Debug.toString)
        , y2 (y_+0.5*hei|>Debug.toString)
        , stroke color_crys_stroke
        , strokeWidth "0.6px"
        ]
        []
      , line
        [ x1 (x_+1.11*wid|>Debug.toString)
        , y1 (y_+0.5*hei|>Debug.toString)
        , x2 (x_+1.25*wid|>Debug.toString)
        , y2 (y_+0.5*hei|>Debug.toString)
        , stroke color_crys_stroke
        , strokeWidth "0.6px"
        ]
        []
      , line
        [ x1 (x_+wid|>Debug.toString)
        , y1 (y_ - 0.05*hei|>Debug.toString)
        , x2 (x_+wid|>Debug.toString)
        , y2 (y_+ 1.05*hei|>Debug.toString)
        , stroke (toString(rgb 80 0 0))
        , strokeWidth "2px"
        ]
        []



    ]



type alias Ball =
    { x : Float
    , y : Float
    , r : Float
    , attribute : Elf_attribute
    }
ballini =
    { x = 450
    , y = 460
    , r = 10
    , attribute=Kindness
    }


ball1tosvg ball=
    [circle
             [ cx (ball.x|>Debug.toString)
             , cy (ball.y|>Debug.toString)
             , r (ball.r|>Debug.toString)
             , fill "url(#ball1)"
             ]
             []]

ball2tosvg ball=
    [circle
             [ cx (ball.x|>Debug.toString)
             , cy (ball.y|>Debug.toString)
             , r (ball.r|>Debug.toString)
             , fill "url(#ball2)"
             ]
             []]

ball3tosvg ball=
    [circle
             [ cx (ball.x|>Debug.toString)
             , cy (ball.y|>Debug.toString)
             , r (ball.r|>Debug.toString)
             , fill "url(#ball3)"
             ]
             []]

ball4tosvg ball=
    [circle
             [ cx (ball.x|>Debug.toString)
             , cy (ball.y|>Debug.toString)
             , r (ball.r|>Debug.toString)
             , fill "url(#ball4)"
             ]
             []]