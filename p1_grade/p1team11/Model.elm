module Model exposing (..)

import Iterate
import Json.Encode as Encode exposing (..)
import Random

{-
type alias Model =
    { window : Window
    , paddle : Paddle
    , ball : Ball
    , block : List Block
    , state : State
    , moveLeft : Bool
    , moveRight : Bool
    , direction : AnimationState
    , reset : Bool
    , collision : Collision
    , mode : Mode
    , stage : Int
    , seed : Random.Seed
    , indexes : ( Int, Int )
    }
-}
type alias Model =
    { window : Window
    , paddle : Paddle
    , ball : Ball
    , block : List Block
    , state : State
    , mode : Mode
    , moveLeft : Bool
    , moveRight : Bool
    , direction : AnimationState
    , reset : Bool
    , collision : Collision
    , size : ( Float, Float )
    , stage:Int
    , trueEnding: Bool
    ----
    , seed : Random.Seed
    , indexes : ( Int, Int )
    , prop : Prop
    }

type alias AnimationState =
    Maybe
        { active : Bool
        , elapsed : Float
        }


type Collision
    = No
    | TopWall
    | RightWall
    | LeftWall
      --| MidPaddle
      --| LeftPaddle
    | OnPaddle
    | TopLeftBlocks
    | TopRightBlocks
    | BottomLeftBlocks
    | BottomRightBlocks

type State
    = Playing
    | Stopped
    | Win
    | Lose
    | StartScreen
    --| ScreenSetting
    | ModeSetting
    | HelpScreen
    ---
    --| Paused


type Mode
    = Story
    | Random


type alias Window =
    { background : String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    }

type ScreenSize
    = Small
    | Medium
    | Large

type PaddleMode
    = Normal Paddle
    | Long Paddle


type alias Paddle =
    { background : String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    , vx : Float
    , vy : Float
    }


type alias Ball =
    { background : String
    , cx : Float
    , cy : Float
    , radius : Float
    , vx : Float
    , vy : Float
    }

type alias Prop =
    { ----
      cx : Float
    , cy : Float
    , vx : Float
    , vy : Float
    , kind : String
    }
type BlockType
    = Ordinary
    | Decelerate
    | Accelerate
    | StageClear
    | BrickWhite
    | BrickBrown
    | BrickVertex
    | Immortal1
    | Immortal2

{-
type alias Block =
    { background : String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    , stroke : String
    , strokeWidth : Float
    , blocktype : BlockType
    }
-}
type alias Block =
    { x : Float
    , y : Float
    , blocktype : BlockType
    }


{- decode : Decode.Decoder Model
   decode =
       Decode.map8
           (\paddleX paddleVx ballX ballY ballVx ballVy Block state ->
               { initial
                   | paddle =
                   , paddle = initialPaddle
                   , ball = initialBall
                   , block = initialBlock
                   , state = Stopped
                   , moveLeft = False
                   , moveRight = False
                   , direction = Nothing
               }
           )
           (Decode.field "paddleX" Decode.float)
           (Decode.field "paddleVx" Decode.float)
           (Decode.field "ballX" Decode.float)
           (Decode.field "ballY" Decode.float)
           (Decode.field "ballVx" Decode.float)
           (Decode.field "ballVy" Decode.float)
           (Decode.field "Block" (Decode.map decodeList decodeblockx))
           (Decode.field "state" (Decode.map decodeState Decode.string))
-}


encode : Int -> Model -> String
encode indent model =
    Encode.encode
        indent
        (Encode.object
            [ ( "paddleX", Encode.float model.paddle.x )
            , ( "paddleVx", Encode.float model.paddle.vx )
            , ( "ballX", Encode.float model.ball.cx )
            , ( "ballY", Encode.float model.ball.cy )
            , ( "ballVx", Encode.float model.ball.vx )
            , ( "ballVy", Encode.float model.ball.vy )
            , ( "Block", encodeList encodeblockx model.block )
            , ( "state", Encode.string (encodeState model.state) )
            ----
            , ( "propX", Encode.float model.prop.cx )
            , ( "propY", Encode.float model.prop.cy )
            , ( "propVx", Encode.float model.prop.vx )
            , ( "propVy", Encode.float model.prop.vy )
            ]
        )


encodeList : (Block -> Value) -> List Block -> Value
encodeList block listblock =
    let
        encodeblock { x, y } =
            Encode.object
                [ ( "blockX", Encode.float x )
                , ( "blockY", Encode.float y )
                ]
    in
    Encode.list encodeblock listblock



{- decodeblockx : Decode.Decoder Block
   decodeblockx =
       Decode.map2 Block
           (Decode.float 0)
           (Decode.index 0 Decode.float)
-}


encodeblockx : Block -> Value
encodeblockx block =
    Encode.list Encode.float [ block.x, block.y ]


decodeState : String -> State
decodeState string =
    case string of
        "playing" ->
            Playing

        "win" ->
            Win

        "lose" ->
            Lose
-----


        _ ->
            Stopped


encodeState : State -> String
encodeState state =
    case state of
        Playing ->
            "playing"

        Stopped ->
            "stopped"

        Win ->
            "win"

        Lose ->
            "lose"

        StartScreen ->
            "start screen"

        --ScreenSetting ->
        --    "screen setting"

        ModeSetting ->
            "game mode"

        HelpScreen ->
            "Help"

-- Init


windowwid =
    1000


windowhei =
    750


windowx =
    250


windowy =
    50



-- Set the coordinates of the window

{-
initial : Model
initial =
    let
        ( index1, index2, seed ) =
            random (Random.initialSeed 0)

        indexes =
            ( index1, index2 )
    in
    { window = initialWindow
    , paddle = initialPaddle
    , ball = initialBall
    , block = initialBlock indexes
    , state = Stopped
    , moveLeft = False
    , moveRight = False
    , direction = Nothing
    , reset = False
    , collision = No
    , mode = Random
    , stage = 0
    , seed = seed
    , indexes = indexes
    }
-}
----
propVx1 : (Int, Int) -> Float
propVx1 indexes =
    toFloat(Tuple.first indexes)/100

initial : Model
initial =
        let
            ( index1, index2, seed ) =
                random (Random.initialSeed 0)

            indexes =
                ( index1, index2 )
        in
    { window = initialWindow
    , paddle = initialPaddle
    , ball = initialBall
    , block = initialBlock
    , state = StartScreen
    , mode = Random
    , moveLeft = False
    , moveRight = False
    , direction = Nothing
    , reset = False
    , collision = No
    , size = (0, 0)
    , stage = 0
    , trueEnding = False
    ----
    , seed = seed
    , indexes = indexes
    , prop = initialProp indexes

    }


initialWindow : Window
initialWindow =
    { background = "white"
    , x = windowx
    , y = windowy
    , width = windowwid
    , height = windowhei
    }

initialBlock: List Block
initialBlock =
    initialBlock0++initialBlock1++initialBlock2++initialBlock3++initialBlock4++initialBlock5++initialBlock6
    ++initialBlock7++initialBlock8++initialBlock9++initialBlock10++initialBlock11++initialBlock12
    ++initialBlock13++initialBlock14++initialBlock15++initialBlock16++initialBlock17++initialBlock18

initialPaddle : Paddle
initialPaddle =
    { background = "#5e2121"
    , x = windowx + (windowwid - 200) / 2
    , y = windowy + windowhei + 60
    , width = 200.0
    , height = 10.0
    , vx = 0
    , vy = 0
    }


initialBall : Ball
initialBall =
    { background = "#8B392C"
    , cx = windowx + windowwid / 2
    , cy = windowy + windowhei - 7 + 60
    ----
    , radius = 8
    , vx = 1
    , vy = -1.5
    }

initialProp : (Int,Int)-> Prop
initialProp indexes =
      { ----
        cx = 720
      , cy = 400
      , vx = propVx1 indexes
      , vy = 1.125
      , kind =  "bigger"
      }

{-| Initialize all the blocks
-}
{-
initialBlock : ( Int, Int ) -> List Block
initialBlock indexes =
    let
        original =
            initialRowBlock
                ++ Iterate.iterate moveBlockYY 1 initialRowBlock
                ++ Iterate.iterate moveBlockYY 2 initialRowBlock
                ++ Iterate.iterate moveBlockYY 3 initialRowBlock
                ++ Iterate.iterate moveBlockYY 4 initialRowBlock

        withAccelerationBlock =
            initialBlockType (Tuple.first indexes) Accelerate original
    in
    initialBlockType (Tuple.second indexes) Decelerate withAccelerationBlock
-}


resolveIndexBlock : Maybe ( Int, Block ) -> ( Int, Block )
resolveIndexBlock target =
    let
        default =
            initialOneBlock ( 0, 0 )
    in
    case target of
        Just a ->
            a

        Nothing ->
            ( 0, default )

{-}
{-| Change the block attributes according to a given block type
-}
changeblock : BlockType -> Block -> Block
changeblock blockType block =
    case blockType of
        Ordinary ->
            block

        Accelerate ->
            { block | blocktype = Accelerate, background = "yellow" }

        Decelerate ->
            { block | blocktype = Decelerate, background = "green" }

        StageClear ->
            { block | blocktype = StageClear, background = "blue" }
-}

{-| Change the type of a specific block in the list with its index
-}
initialBlockType : Int -> BlockType -> List Block -> List Block
initialBlockType num blockType block =
    let
        indblock =
            List.indexedMap Tuple.pair block

        ( front, back ) =
            List.partition (\x -> Tuple.first x < num) indblock

        target =
            List.head back

        targetblock =
            Tuple.second (resolveIndexBlock target)

        newblock =
            changeBlockType blockType targetblock

        ( backfirst, filterdback ) =
            List.partition (\x -> Tuple.first x == num) back

        ( bacblocktypeex, backblock ) =
            List.unzip filterdback

        ( frontindex, frontblock ) =
            List.unzip front
    in
    List.concat [ frontblock, [ newblock ], backblock ]


{-| Initialize an ordinary block
-}
{-initialOneBlock : ( Float, Float ) -> Block
initialOneBlock ( x, y ) =
    { background = "#a32b47"
    , x = windowx + x
    , y = windowy + y
    , width = windowwid / 10
    , height = windowwid / 20
    , stroke = "white"
    , strokeWidth = 1.0
    , blocktype = Ordinary
    }


{-| Move a block with a distance of 1/10 of the window width
-}
moveBlockX : Block -> Block
moveBlockX block =
    { block | x = block.x + windowwid / 10 }


{-| Move a block with a distance of 1/20 of the window height
-}
moveBlockY : Block -> Block
moveBlockY block =
    { block | y = block.y + windowwid / 20 }


{-| Move the whole row of blocks
-}
moveBlockYY : List Block -> List Block
moveBlockYY block =
    List.map moveBlockY block


initialRowBlock : List Block
initialRowBlock =
    Iterate.copy moveBlockX 9 (initialOneBlock ( 0, 0 ))-}

------------
random : Random.Seed -> ( Int, Int, Random.Seed )
random seed =
    let
        ( times, seed1 ) =
            Random.step (Random.int 0 100) seed

        seed2 =
            iterator times seed1

        ( index1, seed3 ) =
            Random.step (Random.int -100 100) seed2

        ( index2, seed4 ) =
            Random.step (Random.int -100 100) seed3
    in
    ( index1, index2, seed4 )


iterator : Int -> Random.Seed -> Random.Seed
iterator times seed =
    case times of
        0 ->
            seed

        _ ->
            let
                ( _, newSeed ) =
                    Random.step (Random.int 0 100) seed
            in
            iterator (times - 1) newSeed



--initialize all the elements

initialOneBlock : ( Float, Float ) -> Block
initialOneBlock ( x, y ) =
    { x = windowx + x - 10
    , y = windowy + y
    , blocktype = Ordinary
    }

changeBlockType: BlockType->Block->Block
changeBlockType bloktype block =
    {block | blocktype = bloktype}

changeimmortal: Block -> Block
changeimmortal block =
    case block.blocktype of
        BrickVertex ->
            {block | blocktype = Immortal2}
        _ ->
            {block | blocktype = Immortal1}
{-
initialBlock : List Block
initialBlock =
    initialBlock0 ++ initialBlock1 ++ initialBlock2 ++ initialBlock3 ++ initialBlock4 ++ initialBlock5 ++ initialBlock6
    ++ initialBlock7 ++ initialBlock8 ++ initialBlock9 ++ initialBlock10 ++ initialBlock11 ++ initialBlock12
     ++ initialBlock13 ++ initialBlock14 ++ initialBlock15 ++ initialBlock16 ++ initialBlock17 ++ initialBlock18
     -}

initialBlock0: List Block
initialBlock0 =
    List.map (changeBlockType BrickVertex) [ initialOneBlock (466,184)]

initialBlock1: List Block
initialBlock1 =
    List.map (changeBlockType BrickBrown)[ initialOneBlock (430,220),  initialOneBlock (394,256),  initialOneBlock (502,220), initialOneBlock (538,256)]

initialBlock2: List Block
initialBlock2 =
     List.map (changeBlockType BrickWhite)[ initialOneBlock (358,292), initialOneBlock (574,292) ]

initialBlock3 : List Block
initialBlock3 =
    List.map (changeBlockType BrickBrown)[ initialOneBlock (322,328), initialOneBlock (286,364), initialOneBlock (250,400), initialOneBlock (286,436)
    , initialOneBlock (322,472), initialOneBlock (610,328), initialOneBlock (646,364), initialOneBlock (682,400)
    , initialOneBlock (646,436), initialOneBlock (610,472)]

initialBlock4: List Block
initialBlock4 =
     List.map (changeBlockType BrickWhite)[ initialOneBlock (358,508), initialOneBlock (574,508) ]

initialBlock5: List Block
initialBlock5 =
    List.map (changeBlockType BrickBrown)[ initialOneBlock (394,544), initialOneBlock (430,580), initialOneBlock (538,544), initialOneBlock (502,580)]

initialBlock6: List Block
initialBlock6 =
    List.map (changeBlockType BrickVertex) [ initialOneBlock (466,616)]

--
initialBlock7: List Block
initialBlock7 =
    List.map (changeBlockType BrickBrown)[ initialOneBlock (466,168), initialOneBlock (466,152), initialOneBlock (682,384), initialOneBlock (682,368)
    , initialOneBlock (466,600), initialOneBlock (466,584), initialOneBlock (250,384), initialOneBlock (250,368)]

initialBlock8: List Block
initialBlock8 =
    List.map (changeBlockType BrickWhite)[ initialOneBlock (466,136), initialOneBlock (682,352), initialOneBlock (466,568), initialOneBlock (250,352) ]

initialBlock9: List Block
initialBlock9 =
    List.map (changeBlockType BrickBrown)[ initialOneBlock (466,120), initialOneBlock (466,104), initialOneBlock (682,336), initialOneBlock (682,320)
    , initialOneBlock (466,552), initialOneBlock (466,536), initialOneBlock (250,336), initialOneBlock (250,320)]

--
initialBlock10: List Block
initialBlock10 =
    List.map (changeBlockType BrickVertex) [ initialOneBlock (466,88)]

initialBlock11: List Block
initialBlock11 =
    List.map (changeBlockType BrickBrown)[ initialOneBlock (430,124),initialOneBlock (394,160),initialOneBlock (502,124),initialOneBlock (538,160)]

initialBlock12: List Block
initialBlock12 =
     List.map (changeBlockType BrickWhite)[ initialOneBlock (358,196),initialOneBlock (574,196)]

initialBlock13: List Block
initialBlock13 =
    List.map (changeBlockType BrickBrown)[ initialOneBlock (322,232),initialOneBlock (286,268),initialOneBlock (610,232),initialOneBlock (646,268)]

initialBlock14: List Block
initialBlock14 =
    List.map (changeBlockType BrickVertex) [ initialOneBlock (250,304),initialOneBlock (682,304)]

initialBlock15: List Block
initialBlock15 =
    List.map (changeBlockType BrickBrown)[ initialOneBlock (286,340),initialOneBlock (322,376),initialOneBlock (646,340),initialOneBlock (610,376)]

initialBlock16: List Block
initialBlock16 =
     List.map (changeBlockType BrickWhite)[ initialOneBlock (358,412),initialOneBlock (574,412)]

initialBlock17: List Block
initialBlock17 =
    List.map (changeBlockType BrickBrown)[ initialOneBlock (394,448),initialOneBlock (430,484),initialOneBlock (538,448),initialOneBlock (502,484)]

initialBlock18: List Block
initialBlock18 =
    List.map (changeBlockType BrickVertex)[ initialOneBlock (466,520)]