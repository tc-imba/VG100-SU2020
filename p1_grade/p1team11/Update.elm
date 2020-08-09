port module Update exposing (..)


import StoryBoard exposing (..)
import Html.Events exposing (on)
import Messages exposing (..)
import Model exposing (..)
{-import StoryBoard exposing (storyboard)-}


port save : String -> Cmd msg


saveToStorage : Model -> ( Model, Cmd Msg )
saveToStorage model =
    ( model, save (Model.encode 2 model) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize width height ->
            ( { model | size = ( toFloat width, toFloat height ) }
            , Cmd.none
            )
        Start ->
            ( { model
                | state = Playing
              }
            , Cmd.none
            )

        MoveLeft on ->
            ( startMove { model | moveLeft = on }
            , Cmd.none
            )

        MoveRight on ->
            ( startMove { model | moveRight = on }
            , Cmd.none
            )

        Reset on ->
            ( { model
                | reset = on
              }
            , Cmd.none
            )
            ----------

        Tick time ->
            model
                |> animate (min time 5)
                |> saveToStorage


        Story1 ->
                        let
                                            ( index1, index2, seed ) =
                                                random model.seed

                                            indexes =
                                                ( index1, index2 )
                        in

            ( { model
                | state = Playing
                , block = storyboard model.stage
                , ball = initialBall
                , paddle = initialPaddle
                ----------
                , prop = initialProp indexes
              }
            , Cmd.none
            )

        {- ( animate (min time 25) model, Cmd.none) -}
        Noop ->
            ( model, Cmd.none )

        GameMode ->
            ( { model | state = ModeSetting }, Cmd.none )

        --ScreenSize ->
        --    ( { model | state = ScreenSetting }, Cmd.none )

        --SmallScreen ->
        --    ( { model | state = ModeSetting }, Cmd.none )

        --MediumScreen ->
        --    ( { model | state = ModeSetting }, Cmd.none )

        --LargeScreen ->
        --    ( { model | state = ModeSetting }, Cmd.none )

        StoryMode ->
            ( { model | mode = Story, state = Stopped }, Cmd.none )

        OrdinaryMode ->
            ( { model | mode = Random, state = Stopped }, Cmd.none )

        Beginning ->
            ( initial , Cmd.none )

        Help ->
            ( { model | state = HelpScreen }, Cmd.none )
        Ending ->
                    ( { model
                        | trueEnding = True
                      }
                    , Cmd.none
                    )
        Restart ->
                    case model.mode of
                        Story ->
                            let
                                block =
                                    storyboard model.stage
                            in
                            ( { initial
                                | block = block
                                , mode = model.mode
                                , stage = model.stage
                                , state = Stopped
                              }
                            , Cmd.none
                            )

                        Random ->
                            ( { initial
                                | state = Stopped
                              }
                            , Cmd.none
                            )

        Jump ->
                    let
                        stage =
                            model.stage + 1
                    in
                    ( { model
                        | stage = stage
                        , state = Stopped
                      }
                    , Cmd.none
                    )




animate : Float -> Model -> Model
animate elapsed model =
    model
        |> movePaddle elapsed
        |> conductCollision (checkCollision model)
        |> moveBall elapsed
        -------
        |> moveProp elapsed
        |> checkEndGame


direction : Model -> Int
direction { moveLeft, moveRight } =
    case ( moveLeft, moveRight ) of
        ( True, False ) ->
            -1

        ( False, True ) ->
            1

        _ ->
            0


startMove : Model -> Model
startMove model =
    if direction model /= 0 then
        { model | direction = Just { active = True, elapsed = 0 } }

    else
        { model | direction = Nothing }


moveBall : Float -> Model -> Model
moveBall elapsed model =
    case model.state of
        Playing ->
            let
                ball =
                    model.ball

                updatedball =
                    { ball | cx = model.ball.cx + model.ball.vx * elapsed / 2, cy = model.ball.cy + model.ball.vy * elapsed / 2 }
            in
            --if model.ball.cy > 65 then
            --    { model | ball = updatedball }
            --
            --else
            --    model
            { model | ball = updatedball }

        _ ->
            model
-------
moveProp : Float -> Model -> Model
moveProp elapsed model =
    case model.mode of
        Random ->
            case model.state of
                Playing ->
                    let
                        prop =
                            (touchProp model).prop

                        updatedball = (touchProp model).ball
                        updatedprop =
                            { prop |  cx = prop.cx + prop.vx * elapsed,cy = prop.cy + prop.vy * elapsed }

                    in
                    { model | prop = updatedprop, ball = updatedball }
                _->
                    model
        _ -> model

{-| The effect of colliding an acceleration block
-}
accelerationAction : Ball -> Collision -> Ball
accelerationAction ball collision =
    case collision of
        TopLeftBlocks ->
            if ball.vx * ball.vy <0 then
                { ball | vx = 1.5*ball.vx / (abs ball.vx) * (abs ball.vy), vy =1.5* ball.vy / (abs ball.vy) * (abs ball.vx)}
            else
                { ball | vx = 1.5*(-ball.vx) / (abs ball.vx) * (abs ball.vy), vy = 1.5*(-ball.vy )/ (abs ball.vy) * (abs ball.vx)}

        BottomRightBlocks ->
            if ball.vx * ball.vy <0 then
                { ball | vx = 1.5*ball.vx / (abs ball.vx) * (abs ball.vy), vy = 1.5*ball.vy / (abs ball.vy) * (abs ball.vx)}
            else if ball.vx * ball.vy == 0 then
                { ball | vx = -ball.vx*1.5, vy = -ball.vy*1.5}
            else
                { ball | vx = -ball.vx / (abs ball.vx) * (abs ball.vy)*1.5, vy = -ball.vy / (abs ball.vy) * (abs ball.vx)*1.5}


        BottomLeftBlocks ->
                    if ball.vx * ball.vy <0 then
                        { ball | vx = -ball.vx / (abs ball.vx) * (abs ball.vy)*1.5, vy = -ball.vy / (abs ball.vy) * (abs ball.vx)*1.5}
                    else if ball.vx * ball.vy == 0 then
                        { ball | vx = -ball.vx*1.5, vy = -ball.vy*1.5}
                    else
                        { ball | vx = ball.vx / (abs ball.vx) * (abs ball.vy)*1.5, vy = ball.vy / (abs ball.vy) * (abs ball.vx)*1.5}
        TopRightBlocks ->
                    if ball.vx * ball.vy <0 then
                        { ball | vx = -ball.vx / (abs ball.vx) * (abs ball.vy)*1.5, vy = -ball.vy / (abs ball.vy) * (abs ball.vx)*1.5}
                    else
                        { ball | vx = ball.vx / (abs ball.vx) * (abs ball.vy)*1.5, vy = ball.vy / (abs ball.vy) * (abs ball.vx)*1.5}
        _ ->
            ball



{-| The effect of colliding an deceleration block
-}
decelerationAction : Ball -> Collision -> Ball
decelerationAction ball collision =
    case collision of
        TopLeftBlocks ->
            if ball.vx * ball.vy <0 then
                { ball | vx = ball.vx / (abs ball.vx) * (abs ball.vy)/1.5, vy = ball.vy / (abs ball.vy) * (abs ball.vx)/1.5}
            else
                { ball | vx = -ball.vx / (abs ball.vx) * (abs ball.vy)/1.5, vy = -ball.vy / (abs ball.vy) * (abs ball.vx)/1.5}

        BottomRightBlocks ->
            if ball.vx * ball.vy <0 then
                { ball | vx = ball.vx / (abs ball.vx) * (abs ball.vy)/1.5, vy = ball.vy / (abs ball.vy) * (abs ball.vx)/1.5}
            else if ball.vx * ball.vy == 0 then
                { ball | vx = -ball.vx/1.5, vy = -ball.vy/1.5}
            else
                { ball | vx = -ball.vx / (abs ball.vx) * (abs ball.vy)/1.5, vy = -ball.vy / (abs ball.vy) * (abs ball.vx)/1.5}


        BottomLeftBlocks ->
                    if ball.vx * ball.vy <0 then
                        { ball | vx = -ball.vx / (abs ball.vx) * (abs ball.vy)/1.5, vy = -ball.vy / (abs ball.vy) * (abs ball.vx)/1.5}
                    else if ball.vx * ball.vy == 0 then
                        { ball | vx = -ball.vx/1.5, vy = -ball.vy/1.5}
                    else
                        { ball | vx = ball.vx / (abs ball.vx) * (abs ball.vy)/1.5, vy = ball.vy / (abs ball.vy) * (abs ball.vx)/1.5}
        TopRightBlocks ->
                    if ball.vx * ball.vy <0 then
                        { ball | vx = -ball.vx / (abs ball.vx) * (abs ball.vy)/1.5, vy = -ball.vy / (abs ball.vy) * (abs ball.vx)/1.5}
                    else
                        { ball | vx = ball.vx / (abs ball.vx) * (abs ball.vy)/1.5, vy = ball.vy / (abs ball.vy) * (abs ball.vx)/1.5}
        _ ->
            ball



{-| The effect of colliding an ordinary block
-}
ordinaryAction : Ball -> Collision -> Ball
ordinaryAction ball collision =
    case collision of
        TopLeftBlocks ->
            if ball.vx * ball.vy <0 then
                { ball | vx = ball.vx / (abs ball.vx) * (abs ball.vy), vy = ball.vy / (abs ball.vy) * (abs ball.vx)}
            else
                { ball | vx = -ball.vx / (abs ball.vx) * (abs ball.vy), vy = -ball.vy / (abs ball.vy) * (abs ball.vx)}

        BottomRightBlocks ->
            if ball.vx * ball.vy <0 then
                { ball | vx = ball.vx / (abs ball.vx) * (abs ball.vy), vy = ball.vy / (abs ball.vy) * (abs ball.vx)}
            else if ball.vx * ball.vy == 0 then
                { ball | vx = -ball.vx, vy = -ball.vy}
            else
                { ball | vx = -ball.vx / (abs ball.vx) * (abs ball.vy), vy = -ball.vy / (abs ball.vy) * (abs ball.vx)}


        BottomLeftBlocks ->
                    if ball.vx * ball.vy <0 then
                        { ball | vx = -ball.vx / (abs ball.vx) * (abs ball.vy), vy = -ball.vy / (abs ball.vy) * (abs ball.vx)}
                    else if ball.vx * ball.vy == 0 then
                        { ball | vx = -ball.vx, vy = -ball.vy}
                    else
                        { ball | vx = ball.vx / (abs ball.vx) * (abs ball.vy), vy = ball.vy / (abs ball.vy) * (abs ball.vx)}
        TopRightBlocks ->
                    if ball.vx * ball.vy <0 then
                        { ball | vx = -ball.vx / (abs ball.vx) * (abs ball.vy), vy = -ball.vy / (abs ball.vy) * (abs ball.vx)}
                    else
                        { ball | vx = ball.vx / (abs ball.vx) * (abs ball.vy), vy = ball.vy / (abs ball.vy) * (abs ball.vx)}
        _ ->
            ball

{-| Get the block that being collided
-}
getCollidedBlock : List Block -> Ball -> List Block
getCollidedBlock block ball =
    List.filter (\singleBlock -> checkBlockCollision1 ball singleBlock || checkBlockCollision2 ball singleBlock
    || checkBlockCollision3 ball singleBlock|| checkBlockCollision4 ball singleBlock) block


{-| Get the updated situation of the ball after collision
-}
blockCollisionAction : List Block -> Ball -> Collision -> Ball
blockCollisionAction block ball collision =
    let
        collidedBlock =
            getCollidedBlock block ball
    in
    if List.length (List.filter (\x -> x.blocktype == Accelerate) collidedBlock) >= 1 then
        accelerationAction ball collision

    else if List.length (List.filter (\x -> x.blocktype == Decelerate) collidedBlock) >= 1 then
        decelerationAction ball collision

    else
        ordinaryAction ball collision



conductCollision : Collision -> Model -> Model
conductCollision collision model =
    case collision of
        No ->
            model

        TopWall ->
            let
                ball =
                    model.ball

                updatedball =
                    { ball | cy = windowy + ball.radius, vy = -ball.vy }
            in
            { model | ball = updatedball }

        RightWall ->
            let
                ball =
                    model.ball

                updatedball =
                    { ball | cx = windowx + windowwid - ball.radius, vx = -ball.vx }
            in
            { model | ball = updatedball }

        LeftWall ->
            let
                ball =
                    model.ball

                updatedball =
                    { ball | cx = windowx + ball.radius, vx = -ball.vx }
            in
            { model | ball = updatedball }

        OnPaddle ->
            let
                ball =
                    model.ball

                updatedball =
                    { ball
                        | cy = model.paddle.y - model.ball.radius
                        , vy = -ball.vy
                        , vx = toFloat (direction model) + ball.vx
                    }
            in
            { model | ball = updatedball }

        TopLeftBlocks ->
            let
                ball =
                    model.ball

                updatedball =
                    blockCollisionAction model.block ball TopLeftBlocks

                updatedblock =
                    List.filter (\singleBlock -> not (checkBlockCollision1 ball singleBlock)|| singleBlock.blocktype == Immortal1
                    || singleBlock.blocktype == Immortal2) model.block


            in
            { model | ball = updatedball,block = updatedblock
            }

        BottomRightBlocks ->
            let
                ball =
                    model.ball

                updatedball =
                    blockCollisionAction model.block ball BottomRightBlocks

                updatedblock =
                    List.filter (\singleBlock -> not (checkBlockCollision2 ball singleBlock) || singleBlock.blocktype == Immortal1
                    || singleBlock.blocktype == Immortal2) model.block



            in
            { model | ball = updatedball,block = updatedblock
            }

        BottomLeftBlocks ->
            let
                ball =
                    model.ball

                updatedball =
                    blockCollisionAction model.block ball BottomLeftBlocks

                updatedblock =
                    List.filter (\singleBlock -> not (checkBlockCollision3 ball singleBlock)|| singleBlock.blocktype == Immortal1
                    || singleBlock.blocktype == Immortal2) model.block



            in
            { model | ball = updatedball, block = updatedblock
            }

        TopRightBlocks ->
            let
                ball =
                    model.ball

                updatedball =
                    blockCollisionAction model.block ball TopRightBlocks

                updatedblock =
                    List.filter (\singleBlock -> not (checkBlockCollision4 ball singleBlock)|| singleBlock.blocktype == Immortal1
                    || singleBlock.blocktype == Immortal2) model.block



            in
            { model | ball = updatedball,block = updatedblock
            }

movePaddle : Float -> Model -> Model
movePaddle elapsed model =
    case model.state of
        Playing ->
            case model.direction of
                Just state ->
                    { model | direction = Just (activateButton 5 elapsed state) }
                        |> (if state.active then
                                movePaddle_ (direction model)

                            else
                                identity
                           )

                Nothing ->
                    model

        _ ->
            model


movePaddle_ : Int -> Model -> Model
movePaddle_ dx model =
    let
        x =
            model.paddle.x

        x_ =
            x + toFloat dx * 25
    in
    if (x >= windowx + 12.5 && dx == -1) || (x <= windowx + windowwid - initialPaddle.width && dx == 1) then
        let
            updatedpaddle =
                { initialPaddle | x = x_ }
        in
        { model | paddle = updatedpaddle }

    else
        model


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

checkEndGame : Model -> Model
checkEndGame model =
    case model.mode of
        Random ->
            let
                ( index1, index2, seed ) =
                    random model.seed

                indexes =
                    ( index1, index2 )
            in
            if model.reset then
                let
                    new_model =
                        initial
                in
                { new_model | seed = seed, indexes = indexes }

            else if List.isEmpty model.block then
                { model | state = Win }

            else if model.ball.cy >= windowhei + windowy +60 + model.paddle.height then
                { model | state = Lose }

            else
                { model | seed = seed, indexes = indexes }

        Story ->
         let
            ( index1, index2, seed ) =
                                       random model.seed

            indexes =
                                   ( index1, index2 )
         in
            if model.reset == True then
                let

                    new_model =
                        initial
                in
                { new_model | seed = seed, indexes = indexes, prop = initialProp indexes, block = initialBlock }

            else if List.isEmpty (List.filter (\x -> x.blocktype == StageClear) model.block) then
                { model | state = Stopped, stage = model.stage + 1, ball = initialBall, paddle = initialPaddle }

            else if model.ball.cy >= windowhei + windowy + 60 + model.paddle.height then
                { model | state = Lose, ball = initialBall, paddle = initialPaddle }

            else if model.stage == 2 && model.trueEnding  then
                { model | state = Stopped, stage = 3, ball = initialBall, paddle = initialPaddle }

            else
                { model | seed = seed, indexes = indexes }


{-checkEndGame : Model -> Model
checkEndGame model =
    case model.mode of
        Random ->
            if model.reset then
                initial

            else if List.isEmpty (model.block) then
                                    { model | state = Win }

            else if model.ball.cy >= windowhei + windowy + 60 + model.paddle.height then
                { model | state = Lose }

            else
                model
        Story ->
                        if model.reset == True then
                           let

                                new_model =
                                    initial
                            in
                            { new_model |  block = initialBlock  }

                        else if List.isEmpty (List.filter (\x -> x.blocktype == StageClear) model.block) then
                            { model | state = Stopped, stage = model.stage + 1, ball = initialBall, paddle = initialPaddle  }

                        else if model.ball.cy >= windowhei + windowy + 60 + model.paddle.height then
                            { model | state = Lose, ball = initialBall, paddle = initialPaddle }

                        else if model.stage == 2 && model.trueEnding then
                            { model | state = Stopped, stage = 3, ball = initialBall, paddle = initialPaddle}

                        else
                            model-}

{-
checkWidthBlockCollision : Ball -> Block -> Bool
checkWidthBlockCollision ball block =
    (block.x <= ball.cx)
        && (block.x >= ball.cx{- - block.width-})
        && (block.y >= ball.cy{- - block.height-} - ball.radius)
        && (block.y <= ball.cy + ball.radius)

checkHeightBlockCollision : Ball -> Block -> Bool
checkHeightBlockCollision ball block =
    (block.x <= ball.cx + ball.radius)
        && (block.x >= ball.cx{- - block.width-} - ball.radius)
        && (block.y >= ball.cy{- - block.height-})
        && (block.y <= ball.cy)
-}

checkBlockCollision1 : Ball -> Block -> Bool
checkBlockCollision1 ball block =
    (block.x <= ball.cx)
        && (block.x + 35 >= ball.cx)
        && (block.y + 35 >= ball.cy + ball.radius * sqrt(2))
        && (block.y <= ball.cy + ball.radius * sqrt(2))

checkBlockCollision2 : Ball -> Block -> Bool
checkBlockCollision2 ball block =
    (block.x <= ball.cx - 35)
        && (block.x >= ball.cx - 70)
        && (block.y <= ball.cy - 47 - ball.radius * sqrt(2))
        && (block.y > ball.cy - 82 - ball.radius * sqrt(2))

checkBlockCollision3 : Ball -> Block -> Bool
checkBlockCollision3 ball block =
    (block.x <= ball.cx)
        && (block.x + 35 >= ball.cx)
        && (block.y <= ball.cy - 47 - ball.radius * sqrt(2))
        && (block.y >= ball.cy - 82 - ball.radius * sqrt(2))

checkBlockCollision4 : Ball -> Block -> Bool
checkBlockCollision4 ball block =
    (block.x < ball.cx - 35)
        && (block.x >= ball.cx - 70)
        && (block.y + 35 >= ball.cy + ball.radius * sqrt(2))
        && (block.y < ball.cy + ball.radius * sqrt(2))

{-
checkWidthBlocksCollision : Ball -> List Block -> Bool
checkWidthBlocksCollision ball blocks =
    List.any (checkWidthBlockCollision ball) blocks


checkHeightBlocksCollision : Ball -> List Block -> Bool
checkHeightBlocksCollision ball blocks =
    List.any (checkHeightBlockCollision ball) blocks
-}
checkBlocksCollision1 : Ball -> List Block -> Bool
checkBlocksCollision1 ball blocks =
    List.any (checkBlockCollision1 ball) blocks

checkBlocksCollision2 : Ball -> List Block -> Bool
checkBlocksCollision2 ball blocks =
    List.any (checkBlockCollision2 ball) blocks

checkBlocksCollision3 : Ball -> List Block -> Bool
checkBlocksCollision3 ball blocks =
    List.any (checkBlockCollision3 ball) blocks

checkBlocksCollision4 : Ball -> List Block -> Bool
checkBlocksCollision4 ball blocks =
    List.any (checkBlockCollision4 ball) blocks


checkCollision : Model -> Collision
checkCollision model =
    if model.ball.cy < windowy + model.ball.radius then
        TopWall

    else if
        checkBlocksCollision1 model.ball
        (  model.block) then
            TopLeftBlocks

    else if
        checkBlocksCollision2 model.ball
        (  model.block) then
            BottomRightBlocks

    else if
        checkBlocksCollision3 model.ball
        (  model.block) then
            BottomLeftBlocks

    else if
        checkBlocksCollision4 model.ball
        (  model.block) then
            TopRightBlocks

    else if model.ball.cx <= windowx + model.ball.radius then
        LeftWall

    else if model.ball.cx >= windowx + windowwid - model.ball.radius then
        RightWall

    else if
        model.ball.cy
            >= model.paddle.y
            - model.ball.radius
            && model.ball.cx
            >= model.paddle.x
            && model.ball.cx
            <= model.paddle.x
            + model.paddle.width
            && model.ball.vy
            > 0
    then
        OnPaddle

    else
        No

touchProp : Model ->  Model
touchProp model =
    let
      ( index1, index2, seed ) =
                         random model.seed

      indexes =
                         ( index1, index2 )

      prop = model.prop
      updatedkind =
          if prop.kind == "bigger" then
             "smaller"
          else
             "bigger"

      updatedProp = { prop | vx = toFloat(Tuple.first indexes)/100, cx = 720, cy = 400, kind = updatedkind}
    in
      if
        model.prop.cy
            >= model.paddle.y
            - 6 -------
            && model.prop.cx
            >= model.paddle.x
            && model.prop.cx
            <= model.paddle.x
               + model.paddle.width

      then
        let
            ball = model.ball
            -----the background belongs to next prop, so the color reversed
            updatedBall =
                        if model.ball.radius < 16 && prop.kind == "bigger" then
                            { ball | radius = ball.radius * 2.0}
                        else if model.ball.radius > 4 && prop.kind == "smaller" then
                            { ball | radius = ball.radius * 0.5}
                        else
                            model.ball

        in
            { model | ball = updatedBall, prop = updatedProp}
      else if
        model.prop.cy
            >= windowy + windowhei +60 - 6 -------


      then
        { model | prop = updatedProp}



      else
        model

