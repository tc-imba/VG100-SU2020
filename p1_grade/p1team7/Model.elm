module Model exposing (..)
import Message exposing (Msg)
import Outlooks exposing (..)
import Url
import Browser.Navigation as Nav


type alias Point =  --*这个不是给方块用的
    {
        x : Float
    ,   y: Float
    }

type alias Keys =
    {
        enter: Bool
    ,   left: Bool   -- 蓝
    ,   right: Bool
    ,   a: Bool     --金
    ,   d: Bool
    ,   one : Bool
    ,   two : Bool
    ,   three : Bool
    ,   four : Bool
    ,   five : Bool
    ,   six : Bool
    ,   seven : Bool
    ,   eight : Bool
    ,   nine : Bool
    ,   ten : Bool
    }

nokeys: Keys
nokeys =
    Keys False False False False False False False False False False False False False False False

type Page
    = Home
    | Help
    | Game

type State
    = Playing -- 正在进行
    | Stopped -- 结束,等待再次开始(已胜利/失败)


type alias AttackState =
    {
        ongoing: Bool
    ,   success: Bool
    }

ongoingAttack: AttackState
ongoingAttack =
    AttackState True False

overAttack: AttackState
overAttack =
    AttackState False False
type alias Clover =
  {
      leftClover : Bool  -- 判断是否打到
    , rightClover : Bool
    , upClover : Bool
  }



type alias Model =
    { keys : Keys
    , state : State
    , pad_x : Float
    , pad_y : Float
    , pad_angle : Float
    , pad_w : Float

    , gold_x : Float
    , gold_y : Float
    , gold_angle : Float
    , gold_w : Float

    , ball_x : Float
    , ball_y : Float
    , ball_vx : Float
    , ball_vy : Float

    , block_vx : Float
    , block_vy : Float
    , block_x : Float
    , block_y : Float

    , wShell_left : Float
    , wShell_up : Float
    , wShell_right : Float
    , wShell_down : Float


    , clover : Clover
    , emptyLeaves : List (Int, Int)
    , blueLeaves: List (Int, Int)
    , cyanLeaves: List (Int, Int)
    , pinkLeaves: List (Int, Int)
    , redLeaves: List (Int, Int)
    , attack : AttackState

    {-, blueBricks: List (Int, Int)
    , cyanBricks: List (Int, Int)
    , pinkBricks: List (Int, Int)
    , redBricks: List (Int, Int)
    , emptyBricks: List (Int, Int)-}

    , nextBrick : Outlooks.Brick
    , nextPoint : (Int,Int)

    , life: Int
    , max_life : Int
    , exp : Int
    , leaf : Int
    , combo : Int
    , minute : Int
    , second : Int
    , skills_ok : List(Bool)
    , skills_cost : List(Int)

    , key : Nav.Key
    , url : Url.Url
    , page : Page
    , music : Outlooks.Music
    , difficulty: Outlooks.Difficulty
    , se : Outlooks.SE
    , booklet : Outlooks.Page
    , showingpage : String
    }

initial : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
initial flags url key =
    ({
        keys = nokeys
      , state = Stopped
      , pad_x = 37
      , pad_y = 25
      , pad_angle = 0 -- 加速度
      , pad_w = 0

      , gold_x = 37
      , gold_y = 25
      , gold_angle = 180
      , gold_w = 0

      , ball_x = 47
      , ball_y = 35
      , ball_vx = -3.0
      , ball_vy = -3.0

      , block_vx = 3
      , block_vy = 1.5
      , block_x = 10
      , block_y = 10

      , wShell_left = 0
      , wShell_up = 120
      , wShell_right = 240
      , wShell_down = 0

      , clover = Clover False False False

      , emptyLeaves = []
      , blueLeaves =
      [
       (0, 4), (1, 4), (2, 4), (3, 4), (4, 4), (5, 4), (6, 4), (7, 4), (8, 4), (9, 4), (10, 4),
       (0, 5), (1, 5), (2, 5), (3, 5), (4, 5), (5, 5), (6, 5), (7, 5), (8, 5), (9, 5), (10, 5),
       (0, 6), (1, 6), (2, 6), (3, 6), (4, 6), (5, 6), (6, 6), (7, 6), (8, 6), (9, 6), (10, 6),
       (0, 13), (1, 13), (2, 13), (3, 13), (4, 13), (5, 13), (6, 13), (7, 13), (8, 13), (9, 13), (10, 13),
       (0, 14), (1, 14), (2, 14), (3, 14), (4, 14), (5, 14), (6, 14), (7, 14), (8, 14), (9, 14), (10, 14),
       (0, 15), (1, 15), (2, 15), (3, 15), (4, 15), (5, 15), (6, 15), (7, 15), (8, 15), (9, 15), (10, 15)
       ]
       , cyanLeaves = []
       , pinkLeaves = []
       , redLeaves = []
       , attack = ongoingAttack
      , nextBrick = Outlooks.Red
      , nextPoint = (0, 0)

      , life = 5
      , max_life = 5
      , exp = 0
      , leaf = 0
      , combo = 0
      , minute = 0
      , second =  0
      , skills_ok = [False,False,False,False,False,False,False,False,False,False]
      , skills_cost = [10,15,20,25,30,40,50,60,70,80]

      , key = key
      , url = url
      , page = Home
      , music = ReturnOfAncients
      , difficulty = Normal
      , se = Quite
      , booklet = Initi
      , showingpage = page1
    }, Cmd.none)

