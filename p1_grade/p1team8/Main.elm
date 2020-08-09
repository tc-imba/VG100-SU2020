module Main exposing (..)

import Ball exposing (Ball, ballupdate, ballview, initball)
import BigBoss.Boss exposing (initboss, viewboss)
import Bonus.Bonuslist exposing (viewlist)
import Boundary exposing (calboundsize, calindex, callifebarsize, viewboundary)
import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp, onResize)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Lifebar exposing (viewLifebar)
import Model exposing (..)
import Msg exposing (..)
import Paddle exposing (..)
import Prompt exposing (..)
import Random exposing (..)
import Svg
import Svg.Attributes as SvgAttrs
import Task
import Update exposing (..)
import Wall exposing (..)


init : () -> ( Model, Cmd Msg )
init flg =
    let
        ( bigboss, newseed ) =
            initboss ( 0, 0 ) 10 (initialSeed 0)

        initmodel =
            { ballmodel = initball ( 200, 200 ) 1 ( 0.4, 0.3 )
            , wallmodel = initWall 3 4
            , paddlemodel = initPadel ( 0, 0 ) 0
            , windowsize = ( 0, 0 )
            , boundarysize = ( 0, 0 )
            , status = Welcome
            , seed = newseed
            , level = 0
            , promptmodel = initprompt ( 0, 0 ) 0
            , life = 3
            , bigboss = bigboss
            , bonuslist = []
            }
    in
    ( initmodel, Task.perform GetViewPort getViewport )


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


view : Model -> Html Msg
view model =
    let
        ( windw, windh ) =
            model.windowsize

        ( boundw, boundh ) =
            model.boundarysize

        ( panelw, panelh ) =
            calindex model.windowsize

        ( px, py ) =
            ( (windw - boundw) / 2, (windh - boundh) / 2 )

        ( barw, barh ) =
            callifebarsize model.windowsize
    in
    div
        [ style "width" "100%"
        , style "height" "100%"
        , style "position" "fixed"
        , style "left" "0"
        , style "top" "0"
        , style "background" "#000000"
        ]
        [ viewprompt model.status model.windowsize model.promptmodel
        , Svg.svg
            [ SvgAttrs.width (String.fromFloat windw)
            , SvgAttrs.height (String.fromFloat windh)
            , SvgAttrs.viewBox ("0 0 " ++ String.fromFloat panelw ++ " " ++ String.fromFloat panelh)
            ]
            [ Svg.svg
                [ SvgAttrs.width (String.fromFloat boundw)
                , SvgAttrs.height (String.fromFloat barh)
                , SvgAttrs.viewBox ("0 0 " ++ String.fromFloat barw ++ " " ++ String.fromFloat barh)
                ]
                [ viewLifebar ( 0, 0 ) (callifebarsize model.windowsize) model.life ]
            , Svg.svg
                [ SvgAttrs.width (String.fromFloat boundw)
                , SvgAttrs.height (String.fromFloat boundh)
                , SvgAttrs.x "0"
                , SvgAttrs.y (String.fromFloat barh)
                , SvgAttrs.viewBox ("0 0 " ++ String.fromFloat boundw ++ " " ++ String.fromFloat boundh)
                ]
                [ viewboundary ( 0, 0 ) model.boundarysize model.level
                , viewWall model.wallmodel
                , ballview model.ballmodel
                , viewboss model.boundarysize model.status model.paddlemodel model.bigboss
                , viewPadel model.paddlemodel
                , viewlist model.boundarysize model.bonuslist
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        --Changed! If expression.
        [ if model.status == Pause then
            Sub.none

          else
            onAnimationFrameDelta Tick
        , onKeyUp (Decode.map (key False) keyCode)
        , onKeyDown (Decode.map (key True) keyCode)
        , onResize Resize
        ]


key : Bool -> Int -> Msg
key on keycode =
    case keycode of
        37 ->
            Left on

        32 ->
            Space on

        39 ->
            Right on

        13 ->
            Enter on

        86 ->
            Vkey on

        80 ->
            Pkey on

        _ ->
            Noop
