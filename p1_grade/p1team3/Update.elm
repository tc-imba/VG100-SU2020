module Update exposing (..)

import Block exposing (..)
import Browser.Dom exposing (Error, Viewport)
import Collision exposing (..)
import Message exposing (..)
import Model exposing (..)
import Paddle exposing (..)
import Parameters exposing (..)
import Result exposing (Result)
import String



loadLevelInit : Int -> Model -> Model
loadLevelInit n model =
    if n == 1 then
        { model
            | block = map1_
            , currentLevel = 1
            , ball = initBall1
            , pad = initPad
            , waterLevel = 0.0
            , waterLevelRiseSpeed = initWater 1
            , targetPos = initTargetpos1
        }

    else if n == 2 then
        { model
            | block = map2_
            , currentLevel = 2
            , ball = initBall2
            , pad = initPad
            , waterLevel = 0.0
            , waterLevelRiseSpeed = initWater 2
            , targetPos = initTargetpos2
        }

    else
        { model
            | block = map3_
            , currentLevel = 3
            , ball = initBall3
            , pad = initPad
            , waterLevel = 0.0
            , waterLevelRiseSpeed = initWater 3
            , targetPos = initTargetpos3
        }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let
        dasset =
            model.asset + gameParams.dcoin

    in
    case msg of
        ClickStore ->
            ( { model | currentPage = StorePage }, Cmd.none )

        ClickStart ->
            ( { model | currentPage = LevelsPage }, Cmd.none )

        ClickAbout ->
            ( { model | currentPage = AboutPage }, Cmd.none )

        ClickHome ->
            ( { model | currentPage = HomePage }, Cmd.none )

        ClickGame ->
            --Clickpage n
            ( { model | currentPage = GamePage, state = Playing }, Cmd.none )

        UGatk ->
            ( calCoin msg model |> Tuple.second, Cmd.none )

        UGspd ->
            ( calCoin msg model |> Tuple.second, Cmd.none )

        Tick newTime ->
            if model.state == Playing then
                ( model
                    |> waterRise
                    |> updateVeloAndHitBlocks
                    |> checkLevelStatus
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        AddKey keyValue ->
            if model.currentPage == LevelFinish && model.pad.y < negate 24 * gameParams.yStep then
                case keyValue of
                    Control "Enter" ->
                        if model.currentLevel /= 3 then
                            ( { model
                                | currentPage = LevelsPage
                                , asset = dasset
                              } |> loadLevelInit model.currentLevel
                            , Cmd.none
                            )

                        else
                            ( { model
                                | currentPage = EndingCG
                                , asset = dasset
                              } |> loadLevelInit model.currentLevel
                            , Cmd.none
                            )

                    _ ->
                        ( model, Cmd.none )

            else if model.state == Stopped && model.pad.y > negate 16 * gameParams.yStep then
                case keyValue of
                    Control "Enter" ->
                        ( { model
                            | currentPage = LevelsPage
                          } |> loadLevelInit model.currentLevel
                        , Cmd.none
                        )

                    _ ->
                        ( model, Cmd.none )

            else if model.state == Playing then
                case keyValue of
                    Character s ->
                        ( s |> toDirection |> move model, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        Resize w h ->
            ( { model | screenSize = ( toFloat w, toFloat h ) }, Cmd.none )

        GotViewport viewport ->
            ( { model | viewport = Just viewport, screenSize = ( viewport.viewport.width, viewport.viewport.height ) }, Cmd.none )

        ClickLevel lv ->
            ( loadLevelInit lv model, Cmd.none )

        ClickCG ->
            ( { model | currentPage = OpeningCG }, Cmd.none )


        _ ->
            ( model, Cmd.none )


checkLevelStatus : Model -> Model
checkLevelStatus model =
    if model.state == Playing && model.currentLevel <= 3 && model.pad.y < negate 24 * gameParams.yStep && model.currentPage == LevelFinish then
        { model
            | state = Stopped
            , level = updateLevel model.currentLevel model.level
        }

    else if model.state == Playing && model.currentLevel <= 3 && model.pad.y < negate 16 * gameParams.yStep then
        { model | currentPage = LevelFinish }

    else
        model



--=====================================
---------------ASSET UPDATE------------


calCoin : Message.Msg -> Model.Model -> ( Result String Model.Model, Model.Model )
calCoin msg model =
    let
        atkG =
            model.properties.atk

        spdG =
            model.properties.spd

        atkCoin =
            model.asset - atkG * 50

        spdCoin =
            toFloat model.asset - spdG * 50

        p =
            model.properties

        properties1 =
            -- { p | spd = model.properties.spd + updateParams.spdIncrement }
            { p | spd = model.properties.spd + 1 }

        properties2 =
            { p | atk = model.properties.atk + 1 }

        -- log3 =
        --     Debug.toString model.asset
        --         |> Debug.log "the current coin is: "
        -- log4 =
        --     Debug.toString spdCoin
        --         |> Debug.log "the left spdCoin is: "
        -- log5 =
        --     Debug.toString atkCoin
        --         |> Debug.log "the left atkCoin is: "
        -- log6 =
        --     Debug.toString model.properties.spd
        --         ++ Debug.toString model.properties.atk
        --         |> Debug.log "the current spd and atk is: "
    in
    case msg of
        Message.UGatk ->
            if atkG < 4 then
                if atkCoin >= 0 then
                    ( Ok { model | asset = atkCoin, properties = properties2 }, { model | asset = atkCoin, properties = properties2 } )

                else
                    ( Err "You have no coins right now!", model )

            else
                ( Err "You are now full-grade! Cannot upgrade anymore!", model )

        Message.UGspd ->
            if spdG < 4 then
                if spdCoin >= 0 then
                    ( Ok { model | asset = round spdCoin, properties = properties1 }, { model | asset = round spdCoin, properties = properties1 } )

                else
                    ( Err "You have no coins right now!", model )

            else
                ( Err "Youa are now full-grade! Cannot upgrade anymore!", model )

        _ ->
            ( Ok model, model )


move : Model -> Direction -> Model
move model d =
    case d of
        PadLeft ->
            padLeft model

        PadRight ->
            padRight model
        _ ->
            model



--=====================================
