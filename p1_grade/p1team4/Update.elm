port module Update exposing (..)

import Model exposing (..)
import Bricks exposing (..)
import Message exposing (Msg(..))
import Platform.Cmd
import Browser.Navigation



update :  Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
                model
                    |> animate (min time 25)
                    |> saveToStorage
        MoveLeft on ->
                ( startMove { model | board_moveLeft = on }
                , Cmd.none
                )

        MoveRight on ->
                 ( startMove { model | board_moveRight = on }
                 , Cmd.none
                 )

        BallLarger ->
                 let r_ = model.ball.r + 1
                     x_ = model.ball.x
                     y_ = model.ball.y
                     a_ = model.ball.attribute
                     score_ = model.score - 5
                 in
                 if model.score >= 5 then
                     ( { model | ball = { x = x_ , y = y_ , r = r_ , attribute = a_ } , score = score_ }
                     , Cmd.none)
                 else
                     ( model , Cmd.none )
        BallSmaller ->
                 let r_ = model.ball.r - 1
                     x_ = model.ball.x
                     y_ = model.ball.y
                     a_ = model.ball.attribute
                     score_ = model.score - 5
                 in
                 if model.score >= 5 then
                     ( { model | ball = { x = x_ , y = y_ , r = r_ , attribute = a_ } , score = score_ }
                     , Cmd.none )
                 else
                     ( model , Cmd.none )
        BallFaster ->
                 let v0_ = model.ballV0 * 1.1
                     score_ = model.score - 5
                 in
                 if model.score >= 5 then
                    ( { model | ballV0 = v0_ ,score = score_ }
                    , Cmd.none )
                 else
                    ( model , Cmd.none )
        BallSlower ->
                 let v0_ = model.ballV0 * 0.9
                     score_ = model.score - 5
                 in
                 if model.score >= 5 then
                    ( { model | ballV0 = v0_ , score = score_ }
                    , Cmd.none)
                 else
                    ( model , Cmd.none )
        GainChance ->
                 let h_ = model.chance + 1
                     score_ = model.score - 10
                 in
                 if model.score >= 10 then
                    ( { model | chance = h_ , score = score_ }
                    , Cmd.none)
                 else
                    ( model , Cmd.none )
        Noop ->
                ( model, Cmd.none )
        Start ->
                ( { model | state = Playing }
                , Cmd.none)
        Pause ->
                saveToStorage { model | state = Paused }
        Resume ->
               ( { model | state = Playing }
                 , Cmd.none )
        Restart ->
               ( initial_for_restart model
                 , Cmd.none )
        GetViewport { viewport } ->
               ( { model | size = ( viewport.width
                                  , viewport.height
                                  )}
               , Cmd.none
               )
        Resize width height ->
               ( { model | size = ( toFloat width, toFloat height ) }
               , Cmd.none
               )
        BallTypeto1 ->
               ( { model | ballType = 1 }
               |> elfAttributeUpdate_ (elfAttributeUpdate model.ballType)
                 , Cmd.none )
        BallTypeto2 ->
               ( { model | ballType = 2}
               |> elfAttributeUpdate_ (elfAttributeUpdate model.ballType)
                 , Cmd.none )
        BallTypeto3 ->
               ( { model | ballType = 3}
               |> elfAttributeUpdate_ (elfAttributeUpdate model.ballType)
                 , Cmd.none )
        BallTypeto4 ->
               ( { model | ballType = 4}
               |> elfAttributeUpdate_ (elfAttributeUpdate model.ballType)
                 , Cmd.none )
        GoWrath ->
               ( model
               , Browser.Navigation.load "./wrath.html" )
        BacktoMain ->
               ( model
               , Cmd.none )


        AllClear ->
                let
                   bricks_ori = model.bricks
                   bricks_ = List.map (\a -> { a | lives = 0, ifExist = False } ) bricks_ori
               in
               ( { model | bricks = bricks_}, Cmd.none )

        Accelerate on->
                       ({model|accelerate=on},Cmd.none)

        ShowStore ->
            let
                is = model.isStore
                is_ = not is
            in
            ( { model | isStore = is_ }, Cmd.none )







port save : String -> Cmd msg

saveToStorage : Model -> ( Model, Cmd Msg )
saveToStorage model =
    ( model, save (Model.encode 2 model) )

animate : Float -> Model -> Model
animate elapsed model =
    model
        |> moveboard elapsed
        |> ball_fly elapsed
        |> ballHitWall
        |> ballExertedSpellByBoard elapsed
        |> ballHitBoard
        |> ballClearBrickAndBounce
        |> isEndOneTurn
        |> isEndGame
        |> isWin

board_direction : Model -> Int --To prevent unexpected bug happens, I change this to Int (X of board) __Kaiwen Zhang
board_direction { board_moveLeft, board_moveRight } =
    case ( board_moveLeft, board_moveRight ) of
        ( True, False ) ->
            -1

        ( False, True ) ->
            1

        _ ->
            0

startMove : Model -> Model
startMove model =
    if board_direction model /= 0 then
        { model | boarddir = Just { active = True, elapsed = 0 } }

    else
        { model | boarddir = Nothing }

moveboard : Float-> Model-> Model
moveboard elapsed model=
    case model.boarddir of
        Just state ->
            { model | boarddir = Just (activateButton 50 elapsed state) }
             |> (if state.active then
               moveboard_ (board_direction model)

              else
               identity
               )
        Nothing->
            model

moveboard_ : Int-> Model -> Model
moveboard_  dx model =
       let
               x =
                   model.board.x
               x_ = x + dx * 20 * (board_acceleration model)

               x__ =
                   if x_ < 0 then  -- Did you hit the wall
                     x
                   else if x_ > 700 then
                     x
                   else
                     x_
               board = model.board
               board_ = { board | x = x__ }
       in
           { model | board = board_ }


board_acceleration: Model -> Int
board_acceleration model =
    if model.accelerate == True then
    2
    else
    1

activateButton : Float -> Float -> { a | active : Bool, elapsed : Float } -> { a | active : Bool, elapsed : Float }
activateButton interval elapsed state =
    let
        elapsed_ =
            state.elapsed + elapsed
    in
    if elapsed_ > interval then
        { state | active = True, elapsed = elapsed_ - interval }

    else
        { state | active = False, elapsed = elapsed_ }




ball_fly : Float -> Model -> Model
ball_fly elapsed model =
    let
        ( x, y ) =
            ( model.ball.x, model.ball.y )

        ( v_x, v_y ) =
            ( model.ballV.v_x, model.ballV.v_y )

        ( x_, y_ ) =
            ( x + elapsed * v_x, y + elapsed * v_y )

        ball =
            model.ball

        ball_ =
            { ball | x = x_, y = y_ }

    in
    { model | ball = ball_ }

ballHitWall : Model -> Model
ballHitWall model =
    let
        vx = model.ballV.v_x
        absvx = abs model.ballV.v_x
        vx_reverse1 = absvx
        vx_reverse2 = -absvx
        vy=model.ballV.v_y
        vy_reverse = abs model.ballV.v_y
    in

    if model.ball.x <= 10 then
    { model | ballV={ v_x = vx_reverse1 , v_y = vy } }

    else if model.ball.x >=890 then
    { model | ballV={ v_x = vx_reverse2 , v_y = vy } }

    else if model.ball.y <= 10 then
    { model | ballV={ v_x = vx , v_y = vy_reverse } }

    else
    { model | ballV={v_x = vx , v_y = vy } }

ballHitBoard : Model -> Model
ballHitBoard model_ =
    let
         model = ballMoveByBoard model_
         ball = model.ball
         ( board_x, board_y ) =
               ( toFloat model.board.x, model.board.y ) --left up.x is int
         ( wid, hei ) =
            ( model.board.width * 1.25, model.board.height )
         ( o_x, o_y ) =
            ( ball.x, ball.y )
         r = ball.r
         ( v_x, v_y ) = ( model.ballV.v_x, model.ballV.v_y )

         dy = -o_y + board_y  -- Center of the circle to top of the board

         dy_corner_left =  -- center of the circle to the left corner of the board
             (o_x - board_x)^2 + (o_y - board_y)^2
             |> sqrt

         dy_corner_right = -- center of the circle to the right corner of the board
             (o_x - board_x - wid)^2 + (o_y - board_y)^2
             |> sqrt


    in
    if dy >= -hei && dy <= r && o_x >= board_x - r && o_x <= board_x + wid +r then -- 侧壁或角落，圆心横坐标条件

        if o_x >= board_x && o_x <= board_x + wid then -- upper edge
            { model | ballV = { v_x = v_x, v_y = -(abs v_y) } }
        else if dy_corner_left <= r  then -- corners
            { model | ballV = { v_x = -(abs v_x), v_y = -v_y } }
        else if dy_corner_right <= r then
            { model | ballV = { v_x = abs v_x, v_y= -v_y } }
        else
            { model | ballV = { v_x = -v_x, v_y = v_y } } -- side edge
    else
        model_ -- no collide

ballExertedSpellByBoard : Float -> Model -> Model
ballExertedSpellByBoard elapsed model_ =
    let
         model = model_
         ball = model.ball
         ( board_x, board_y ) =
               ( toFloat model.board.x, model.board.y )
         ( wid, hei ) =
            ( model.board.width * 1.25, model.board.height )
         ( o_x, o_y ) =
            ( ball.x, ball.y )
         r = ball.r
         d = 100
         ( v_x, v_y ) = ( model.ballV.v_x, model.ballV.v_y )
         v0 = model.ballV0
         v_y_positive = (2*v0/d) * dy + v0
         v_y_negative = (-2*v0/d) * dy - v0
         v_y_ =
             if (v_y >= 0) then v_y_positive
             else v_y_negative
         dy = -o_y + board_y  -- Center to upper edge

    in
    if (0 <= dy && dy <= d && board_x - r <= o_x && o_x <= board_x + wid + r) then
        { model | ballV = { v_x = v_x, v_y = v_y_} }
    else
        model


ballMoveByBoard : Model -> Model -- As the board moves, moving to the left gives it a velocity to the left, and going to the right gives it a velocity to the right
ballMoveByBoard model =
    let
        ( v_x, v_y ) = ( model.ballV.v_x, model.ballV.v_y )
        ( v_x_, v_y_ ) =
            if model.board_moveLeft then
                ( v_x - 0.05 , v_y )
            else if model.board_moveRight then
                ( v_x + 0.05 , v_y )
            else
                ( v_x, v_y )

    in
    { model | ballV = {v_x = v_x_,v_y = v_y_} }







isHitBrick : Model -> Brick -> Bool -- output crash or not
isHitBrick model abrick =
        let
             ball = model.ball
             ( brick_x, brick_y ) =
                   ( abrick.position
                   |> Tuple.first
                   |> toFloat,
                   abrick.position
                   |> Tuple.second
                   |> toFloat ) -- left up

             ( wid, hei ) =
                ( toFloat abrick.width, toFloat abrick.height )

             ( o_x, o_y ) =
                ( ball.x, ball.y )
             r = ball.r
             ( v_x, v_y ) = ( model.ballV.v_x, model.ballV.v_y)

             dy_up = brick_y - o_y -- upper edge brick-oy
             dy_down = -(dy_up + hei) -- lower edge oy-brick
             dx_left = brick_x - o_x -- left edge, brick-ox
             dx_right = -(dx_left + wid) -- right edge, ox-brick
             dy_corner_lu =
                 (o_x - brick_x)^2 + (o_y - brick_y)^2
                 |> sqrt
             dy_corner_ru =
                 (o_x - brick_x - wid)^2 + (o_y - brick_y)^2
                 |> sqrt
             dy_corner_ld =
                 (o_x - brick_x)^2 + (o_y - brick_y - hei)^2
                 |> sqrt
             dy_corner_rd =
                 (o_x - brick_x - wid)^2 + (o_y - brick_y - hei)^2
                 |> sqrt

        in
        case abrick.ifExist of
            True ->
                if brick_x - r <= o_x && o_x <= brick_x + wid + r && brick_y - r <= o_y && o_y <= brick_y + hei + r then
                    if brick_x <= o_x && o_x <= brick_x + wid then -- up&down
                        if dy_down <= r || dy_down <= r then True
                        else False
                    else if brick_y <= o_y && o_y <= brick_y + hei then -- left&right
                        if dx_left <= r || dx_right <= r then True
                        else False
                    else -- corner
                        if dy_corner_lu <= r || dy_corner_ru <= r || dy_corner_ld <= r || dy_corner_rd <= r then True
                        else False
                else False
            False ->
                False


ballHitBrick : Model -> Brick -> Model --
ballHitBrick model abrick =
    let
                 ball = model.ball
                 ( brick_x, brick_y ) =
                       ( abrick.position
                       |> Tuple.first
                       |> toFloat,
                       abrick.position
                       |> Tuple.second
                       |> toFloat ) -- lu

                 ( wid, hei ) =
                    ( toFloat abrick.width, toFloat abrick.height )

                 ( o_x, o_y ) =
                    ( ball.x, ball.y )
                 r = ball.r
                 ( v_x, v_y ) = ( model.ballV.v_x, model.ballV.v_y)

                 dy_up = brick_y - o_y
                 dy_down = -(dy_up + hei)
                 dx_left = brick_x - o_x
                 dx_right = -(dx_left + wid)
                 dy_corner_lu =
                     (o_x - brick_x)^2 + (o_y - brick_y)^2
                     |> sqrt
                 dy_corner_ru =
                     (o_x - brick_x - wid)^2 + (o_y - brick_y)^2
                     |> sqrt
                 dy_corner_ld =
                     (o_x - brick_x)^2 + (o_y - brick_y - hei)^2
                     |> sqrt
                 dy_corner_rd =
                     (o_x - brick_x - wid)^2 + (o_y - brick_y - hei)^2
                     |> sqrt

            in
            case abrick.ifExist of
                True ->
                    if brick_x - r <= o_x && o_x <= brick_x + wid + r && brick_y - r <= o_y && o_y <= brick_y + hei + r then
                        if brick_x <= o_x && o_x <= brick_x + wid then
                            if dy_down <= r || dy_down <= r then
                                { model | ballV = { v_x = v_x, v_y = -v_y } }
                            else model
                        else if brick_y <= o_y && o_y <= brick_y + hei then
                            if dx_left <= r || dx_right <= r then
                                { model | ballV = { v_x = -v_x, v_y = v_y } }
                            else model
                        else
                            if dy_corner_lu <= r || dy_corner_ru <= r || dy_corner_ld <= r || dy_corner_rd <= r then
                                { model | ballV = { v_x = -v_x, v_y = -v_y } }
                            else model
                    else model
                False ->
                    model

isStateMachineMatched : Elf_attribute -> Brick_attribute -> Bool
isStateMachineMatched elf_att bri_att =
    if (elf_att == Humility && bri_att == Pride
    || elf_att == Kindness && bri_att == Envy
    || elf_att == Charity && bri_att == Greed
    || elf_att == Patience && bri_att == Wrath) then
        True
    else
        False

brickDamaged : Model -> Brick -> Brick
brickDamaged model brick =
    let
        isCollided = isHitBrick model brick
        isexist = brick.ifExist
        elfAtt = model.ball.attribute
        brickAtt = brick.attribute
    in
    if isCollided then
        if isexist then
            let
                lives_ = brick.lives
            in
            if isStateMachineMatched elfAtt brickAtt then
                { brick | lives = 0 }
            else
              case brick.attribute of
                 Pride -> { brick | lives = lives_ - 1 , color ="url(#base5)" }
                 Envy -> { brick | lives = lives_ - 1 , color ="url(#base6)" }
                 Greed -> { brick | lives = lives_ - 1 , color ="url(#base7)" }
                 Wrath -> { brick | lives = lives_ - 1 , color ="url(#base8)" }
        else brick
    else brick

oneBrickDisappear : Brick -> Brick
oneBrickDisappear brick_ =
    if brick_.ifExist then
        case brick_.lives of
            0 -> { brick_ | ifExist = False }
            _ -> brick_
    else brick_

bricksDisappear : List Brick -> List Brick
bricksDisappear bricks =
    let
        bricksAfterDisappear = List.map oneBrickDisappear bricks
    in
    bricksAfterDisappear

ballClearBrickAndBounce : Model -> Model
ballClearBrickAndBounce model =
    let
        collideBrick = List.filter (isHitBrick model) model.bricks
        bricksAfterCollide = List.map (brickDamaged model) model.bricks |> bricksDisappear
        isCollided = not (List.isEmpty collideBrick)
        modelAfterCollide = { model | bricks = bricksAfterCollide }
        reboundModel = ballHitBrick modelAfterCollide (Maybe.withDefault abrick (List.head collideBrick))
        score = reboundModel.score
    in
        if not isCollided then
            model
        else
            { reboundModel | score = score + 1 } -- Add rebound effect. Pick the first one in the list to calculate the rebound of the ball


isEndOneTurn : Model-> Model
isEndOneTurn model =
    let
        chance = model.chance
        x = toFloat model.board.x
        width = model.board.width
        y = model.board.y - 10
        x_ = x + width/2
        attribute=model.ball.attribute
        ballV = model.ballV
    in
    if model.ball.y>510 then
        { model | state = Paused, chance = chance - 1, ball = { x = x_, y = y, r = 10,attribute=attribute}, ballV = { ballV | v_x = 0 } }
    else
        model


isEndGame: Model->Model
isEndGame model=
    let
        chance=model.chance
    in
    if chance == 0 then
    { model | state=End }
    else
    model

ifBrickExist: Brick -> Bool
ifBrickExist brick =
    brick.ifExist

isWin: Model -> Model
isWin model =
    let
       existBricks = List.filter ifBrickExist model.bricks
       l = List.length existBricks
    in
    if l == 0 then
    {model | state=Win }
    else
    model

elfAttributeUpdate: Int-> Elf_attribute
elfAttributeUpdate attributeint=
    case attributeint of
        1 -> Humility
        2 -> Kindness
        3 -> Charity
        _ -> Patience --changed order by zhouyuxiang 6/11

elfAttributeUpdate_: Elf_attribute-> Model-> Model
elfAttributeUpdate_ attribute model=
    let
        x=model.ball.x
        y=model.ball.y
        r=model.ball.r
    in
     { model |  ball = { x = x, y = y, r = r,attribute=attribute} }