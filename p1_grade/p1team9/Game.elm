module Game exposing (..)
import Random exposing (Seed)




type alias GameModel =
    {  lis: Grid
     , wid: Float
     , hei: Float
     , bal: List Ball
     , bod: Board
     , sta: Status
     , con: Country
     , ene: Country
     , tur: Int
     , see: Random.Seed
     , ski: Float
     , swt: Bool
     , stp: Int
    }


type alias Grid =
    List Block


type alias Ball =
    {  col : Color
     , vel : Float
     , rad : Float
     , ang : Int
     , x   : Float
     , y   : Float
    }


type alias Board =
    {  col   : Color
     , vel   : Float
     , wid   : Float
     , hei   : Float
     , x     : Float
     , y     : Float
     , left  : Bool
     , right : Bool
    }


type Status =
         Playing
       | Paused
       | GG
       | Empty
       | Win


type Country =
          Rome
        | Persia
        | Goth
        | No


type alias Block =
    {  col : Color
     , i   : Int          -- 行脚标
     , j   : Int
     , sta : BlockStatus  --上面
     , sha : Int          --几变形
     , typ : Int          --颜色角标
     , ang : Int          --初始角度
     , len : Float        --边长
     , rad : Float        --半径
     , x   : Float
     , y   : Float
    }


type BlockStatus =
         Normal
       | Vanished  --消失成原来的颜色
       | Darken    --加深
       | Fade      --消失


type alias Color =
    {  red   : Float
     , green : Float
     , blue  : Float
    }                     --红绿蓝的record


rgb : Int -> Int -> Int -> Color     --
rgb red green blue =
    Color (toFloat red) (toFloat green) (toFloat blue)


col_bank        =   (rgb 109 7  125 )       --前面元素，后面List
                  ::(rgb 109 7  125 )
                  ::(rgb 109 7  125 )
                  ::(rgb 110 173 61 )
                  ::(rgb 110 173 61 )
                  ::(rgb 110 173 61 )
                  ::(rgb 22  80  142)
                  ::(rgb 22  80  142)
                  ::(rgb 22  80  142)
                  ::(rgb 65  121 11 )
                  ::(rgb 65  121 11 )
                  ::(rgb 65  121 11 )
                  ::(rgb 182 4   4  )
                  ::(rgb 182 4   4  )       -- ::并到list里面
                  ::(rgb 182 4   4  )
                  ::[]                      --最后是一个list


n_ =
    {  i_3 = 8
     , i_4 = 6
     , i_6 = 10
     , j_3 = 7
     , j_4 = 11
     , j_6 = 6
     , wid = 450
    }


ball_rome : Ball
ball_rome =
    Ball (rgb 248 247 248) (850 / 140) 10 133 0 -90


ball_persia : Ball
ball_persia =
    Ball (rgb 248 247 248) (725 / 140) 10 133 0 -90


ball_goth : Ball
ball_goth =
    Ball (rgb 248 247 248) (600 / 140) 10 133 0 -90


board_rome : Board
board_rome =
    Board (rgb 231 197 88) 7 100 7 0 -120 False False


board_persia : Board
board_persia =
    Board (rgb 231 197 88) 6 100 7 0 -120 False False


board_goth : Board
board_goth =
    Board (rgb 231 197 88) 5 100 7 0 -120 False False


empty: Grid
empty = []


tri_init : Grid
tri_init =
    initialList n_.wid 600 empty 0 0  (n_.i_3 - 1) (n_.j_3 - 1) 3 True (Random.initialSeed 2344) 1


sqr_init : Grid
sqr_init =
    initialList n_.wid 600 empty 0 0  (n_.i_4 - 1) (n_.j_4 - 1) 4 True (Random.initialSeed 2399) 1


hex_init : Grid
hex_init =
    initialList n_.wid 600 empty 0 0  (n_.i_6 - 1) (n_.j_6 - 1) 6 False (Random.initialSeed 2333) 1


sqr_secd : Grid
sqr_secd =
    initialList n_.wid 600 empty 0 0  (n_.i_4 - 1) (n_.j_4 - 1) 4 True (Random.initialSeed 2399) 2


hex_secd : Grid
hex_secd =
    initialList n_.wid 600 empty 0 0  (n_.i_6 - 1) (n_.j_6 - 1) 6 True (Random.initialSeed 2333) 2

tri_secd : Grid
tri_secd =
    initialList n_.wid 600 empty 0 0  (n_.i_3 - 1) (n_.j_3 - 1) 3 False (Random.initialSeed 2344) 2


rome_init : GameModel
rome_init =
    GameModel hex_init n_.wid 600 [ball_persia] board_persia Empty Rome Persia 1 (Random.initialSeed 2) 0 False 2


persia_init : GameModel
persia_init =
    GameModel tri_init n_.wid 600 [ball_goth] board_goth Empty Persia Goth 1 (Random.initialSeed 2) 0 False 3


goth_init : GameModel
goth_init =
    GameModel sqr_init n_.wid 600 [ball_rome] board_rome Empty Goth Rome 1 (Random.initialSeed 0) 0 False 1


rome_secd : GameModel
rome_secd =
    GameModel tri_secd n_.wid 600 [ball_goth] board_goth Empty Rome Goth 2 (Random.initialSeed 2) 0 False 2


persia_secd : GameModel
persia_secd =
    GameModel sqr_secd n_.wid 600 [ball_rome] board_rome Empty Persia Rome 2 (Random.initialSeed 0) 0 False 3

goth_secd : GameModel
goth_secd =
    GameModel hex_secd n_.wid 600 [ball_persia] board_persia Empty Goth Persia 2 (Random.initialSeed 2) 0 False 1


initialList : Float -> Float -> Grid -> Int -> Int -> Int -> Int -> Int -> Bool -> Seed -> Int -> Grid
initialList wid hei grid i_temp j_temp i_max j_max shape bomb seed level =

    let
        (num,newSeed) =
            if bomb then
                Random.step (Random.int 0 (List.length col_bank - 1)) seed
            else
                Random.step (Random.int 3 (List.length col_bank - 1)) seed

    in

    let num_ =
            if (num>=9 && num<12 && level ==1 ) then
                num - 6
            else if (num>=3 && num<6 && level ==2 ) then
                num + 6
            else
                num
    in

    let
        color =
            Maybe.withDefault (rgb 124 21  141)  (List.head(List.drop num_ col_bank))
    in

    let
        dir =
            if modBy 4 i_temp == 1 || modBy 4 i_temp == 2 then
                1
            else
                0
    in

    let
        ang =
            if shape == 3 then
                round(30 + dir * 180)
            else if shape == 4 then
                round(45 + dir * 180)
            else if shape == 6 then
                round(0 + dir * 180)
            else
                round(0 + dir * 180)
    in

    let
        n =
            if shape == 3 then
                n_.j_3
            else if shape == 4 then
                n_.j_4
            else if shape == 6 then
                n_.j_6
            else
                n_.j_6
    in

    let
        len =
            if shape == 3 then
                wid / (n - 0.5)
            else if shape == 4 then
                wid / n
            else if shape == 6 then
                wid / (3 * n - 1.5)
            else
                wid / (3 * n - 1.5)
    in

    let
        rad =
            if shape == 3 then
                len / sqrt 3
            else if shape == 4 then
                len / sqrt 2
            else if shape == 6 then
                len
            else
                len
    in

    let
        x =
            if shape == 3 then
                -wid / 2 + 0.5 * len * toFloat((modBy 2 i_temp)) + len * toFloat(j_temp)
            else if shape == 4 then
                -wid / 2 + len / 2 + len * toFloat(j_temp)
            else if shape == 6 then
                -wid / 2 + 1.5 * len * toFloat((modBy 2 i_temp )) + 3 * len * toFloat(j_temp)
            else
                -wid / 2 + 1.5 * len * toFloat((modBy 2 i_temp)) + 3 * len * toFloat(j_temp)
    in

    let
        y =
            if shape == 3 then
                hei / 2 - (sqrt(3) / 6 * len) - (sqrt(3) / 6 * len * dir) - (sqrt(3) / 2 * len * toFloat(i_temp // 2))
            else if shape == 4 then
                hei / 2 - len / 2 - len * toFloat(i_temp)
            else if shape == 6 then
                hei / 2 - (sqrt(3) / 2 * len * toFloat(i_temp))
            else
                hei / 2 - (sqrt(3) / 2 * len * toFloat(i_temp))
    in

    let
        newBlock =
            Block color i_temp j_temp Normal shape num_ ang len rad x y
    in

    let
        newGrid =
            newBlock::grid
    in


    if i_temp == i_max && j_temp == j_max then
        newGrid
    else if j_temp == j_max then
        initialList wid hei newGrid (1 + i_temp) 0 i_max j_max shape bomb newSeed level
    else
        initialList wid hei newGrid i_temp (1 + j_temp) i_max j_max shape bomb newSeed level


