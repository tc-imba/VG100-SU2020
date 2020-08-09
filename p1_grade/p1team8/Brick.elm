module Brick exposing (..)

import Ball exposing (..)
import Bonus.Bonus exposing (Bonus, Bonustype(..), initbonus)
import Color exposing (..)
import Html exposing (..)
import Msg exposing (..)
import Svg
import Svg.Attributes as SvgAttrs



--This code has not been tested yet, so be careful with it.
--This is a substitution of the ball model, just to make the whole code compilable.
--This is an abbreviation of "Touch side", ust for the touch function to return info about which side the ball touches.


type Tside
    = Up
    | Down
    | Left
    | Right
    | Noop



--The brick model. Field pos locates the whole brick, and it is the coordination of the top left corner of the brick.
--The first float in the pos represents the x component (horizontal, pointing left) and the second float is the y component (vertical, pointing downwards)
--Field size is similar to the field pos. The first float represents the horizontal length and the second float represents the vertical length.
--The field valid records whether the brick is already broken down. If it is, the update and view function of the brick won't do anything anymore.
--The field color is a string representing the color of the brick using RGB color standard.
--The field life records the number of life of the brick remained, since we probably can design a game with all the bricks can be hit twice or three times.


type BrickState
    = Nonexist
    | Disp
    | Killed


type alias BrickModel =
    { pos : ( Float, Float )
    , size : ( Float, Float )
    , valid : BrickState
    , bonustaken : Bool
    , bonustype : Bonustype
    , color : ColorModel
    , shade : ColorModel
    , life : Int
    , number : ( Float, Float )
    }



--Init the basic info of the brick (not including the size)


initbrick : ( Float, Float ) -> ( Float, Float ) -> BrickModel
initbrick ( x, y ) ( w, h ) =
    { pos = ( x, y )
    , size = ( w, h )
    , valid = Disp
    , life = 2
    , color = rgb 71 187 14
    , shade = rgb 0 0 0
    , number = ( x, y )
    , bonustype = Noeffect
    , bonustaken = False
    }



--view function


viewbrick : BrickModel -> Html Msg.Msg
viewbrick model =
    let
        ( x, y ) =
            model.pos

        ( w, h ) =
            model.size

        color =
            model.color
    in
    if model.valid == Disp then
        --if the brick is still there (has not been hit too much that it vanishes)
        {- Svg.rect
           [ SvgAttrs.x (String.fromFloat x)
           , SvgAttrs.y (String.fromFloat y)
           , SvgAttrs.width (String.fromFloat w)
           , SvgAttrs.height (String.fromFloat h)
           , SvgAttrs.fill (colortoString model.color)
           , SvgAttrs.stroke "#153a7a"
           , SvgAttrs.strokeWidth "1"
           ]
           []
        -}
        Svg.svg []
            [ Svg.rect
                [ SvgAttrs.x (String.fromFloat (x + 0.1 * w))
                , SvgAttrs.y (String.fromFloat (y + 0.15 * h))
                , SvgAttrs.width (String.fromFloat w)
                , SvgAttrs.height (String.fromFloat h)
                , SvgAttrs.fill (colortoString model.shade)
                , SvgAttrs.opacity "0.5"
                ]
                []
            , Svg.rect
                [ SvgAttrs.x (String.fromFloat x) --(x+0.1*w))
                , SvgAttrs.y (String.fromFloat y) --(y+0.1*h))
                , SvgAttrs.width (String.fromFloat w) --(0.8*w))
                , SvgAttrs.height (String.fromFloat h) --(0.8*h))
                , SvgAttrs.fill (colortoString model.color)
                ]
                []
            , Svg.rect
                [ SvgAttrs.x (String.fromFloat (x + 0.1 * w))
                , SvgAttrs.y (String.fromFloat (y + 0.2 * h))
                , SvgAttrs.width (String.fromFloat (0.3 * w))
                , SvgAttrs.height (String.fromFloat (0.2 * h))
                , SvgAttrs.fill (colortoString (rgb 255 255 255))
                ]
                []
            ]

    else
        --if it has vanished, then the view function returns nothing.
        Svg.svg [] []


withindist : ( Float, Float ) -> Float -> ( Float, Float ) -> Bool
withindist ( px1, py1 ) r ( px2, py2 ) =
    (px1 - px2) ^ 2 + (py1 - py2) ^ 2 <= r ^ 2


bouncehline : ( Float, Float ) -> Float -> Float -> Bool
bouncehline cpos r y =
    let
        ( cx, cy ) =
            cpos
    in
    abs (cy - y) <= r


bouncevline : ( Float, Float ) -> Float -> Float -> Bool
bouncevline cpos r x =
    let
        ( cx, cy ) =
            cpos
    in
    abs (cx - x) <= r


enterhline : ( Float, Float ) -> Float -> Float
enterhline cpos y =
    let
        ( cx, cy ) =
            cpos
    in
    abs (cy - y)


entervline : ( Float, Float ) -> Float -> Float
entervline cpos x =
    let
        ( cx, cy ) =
            cpos
    in
    abs (cx - x)


touch : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Float -> ( Float, Float ) -> Tside
touch brickpos bricksize ballpos ballrad error =
    let
        ( bx, by ) =
            brickpos

        ( bw, bh ) =
            bricksize

        ( ax, ay ) =
            ballpos

        ( ex, ey ) =
            error

        r =
            ballrad
    in
    --Use the coordination data to decide which relative position are the ball and the brick in.
    if (-2 * ey <= by - (ay + r)) && (by - (ay + r) <= 0) && (bx <= ax + r) && (ax <= bx + bw + r) then
        Up

    else if (-2 * ex <= bx - (ax + r)) && (bx - (ax + r) <= 0) && (ay <= by + bh + r) && (by <= ay - r) then
        Left

    else if (-2 * ex <= (ax - r) - (bx + bw)) && ((ax - r) - (bx + bw) <= 0) && (ay <= by + bh + r) && (by <= ay + r) then
        Right

    else if (-2 * ey <= (ay - r) - (by + bh)) && ((ay - r) - (by + bh) <= 0) && (bx <= ax - r) && (ax <= bx + bw + r) then
        Down

    else if withindist ballpos r brickpos && (enterhline ballpos by < entervline ballpos bx) then
        Left

    else if withindist ballpos r brickpos && (enterhline ballpos by >= entervline ballpos bx) then
        Up

    else if withindist ballpos r ( bx, by + bh ) && (enterhline ballpos (by + bh) < entervline ballpos bx) then
        Down

    else if withindist ballpos r ( bx, by + bh ) && (enterhline ballpos (by + bh) >= entervline ballpos bx) then
        Left

    else if withindist ballpos r ( bx + bw, by ) && (enterhline ballpos by < entervline ballpos (bx + bw)) then
        Right

    else if withindist ballpos r ( bx + bw, by ) && (enterhline ballpos by >= entervline ballpos (bx + bw)) then
        Up

    else if withindist ballpos r ( bx + bw, by + bh ) && (enterhline ballpos (by + bh) < entervline ballpos (bx + bw)) then
        Right

    else if withindist ballpos r ( bx + bw, by + bh ) && (enterhline ballpos (by + bh) >= entervline ballpos (bx + bw)) then
        Down

    else
        Noop


updatebrick : Ball -> BrickModel -> Float -> ( BrickModel, Ball )
updatebrick ball brickmodel elapsed =
    let
        ind =
            max elapsed 25

        ( ballvx, ballvy ) =
            ball.vel

        ( ballx, bally ) =
            ball.pos

        ( brickpx, brickpy ) =
            brickmodel.pos

        ( brickw, brickh ) =
            brickmodel.size

        tside =
            touch brickmodel.pos brickmodel.size ball.pos ball.rad ( abs ballvx * ind, abs ballvy * ind )

        ( ospx, ospy ) =
            ball.vel

        --alther the vel of the ball
        ( nvel, nbpos, nlife ) =
            case tside of
                Up ->
                    ( ( ospx, 0 - abs ospy ), ( ballx, brickpy - ball.rad ), brickmodel.life - 1 )

                Down ->
                    ( ( ospx, abs ospy ), ( ballx, brickpy + brickh + ball.rad ), brickmodel.life - 1 )

                Left ->
                    ( ( 0 - abs ospx, ospy ), ( brickpx - ball.rad, bally ), brickmodel.life - 1 )

                Right ->
                    ( ( abs ospx, ospy ), ( brickpx + brickw + ball.rad, bally ), brickmodel.life - 1 )

                Noop ->
                    ( ( ospx, ospy ), ( ballx, bally ), brickmodel.life )

        --alter the validation
        nvalid =
            if nlife <= 0 then
                Killed

            else
                brickmodel.valid

        newcolor =
            if tside /= Noop then
                setshining 0.7 100 brickmodel.color

            else
                updatecolor elapsed brickmodel.color
    in
    case brickmodel.valid of
        Disp ->
            ( { brickmodel | life = nlife, valid = nvalid, color = newcolor }, { ball | pos = nbpos, vel = nvel } )

        _ ->
            ( brickmodel, ball )


constupdatebrick : Float -> BrickModel -> BrickModel
constupdatebrick elapsed model =
    { model | color = updatecolor elapsed model.color }



--Help change the size of the brick


changesize : ( Float, Float ) -> BrickModel -> BrickModel
changesize nsize model =
    { model | size = nsize }



--Help change the position of the brick


changepos : ( Float, Float ) -> BrickModel -> BrickModel
changepos npos model =
    { model | pos = npos }


brickvalid : BrickModel -> Bool
brickvalid model =
    model.valid == Disp


takebrickbonus : BrickModel -> ( BrickModel, List Bonus )
takebrickbonus model =
    let
        ( x, y ) =
            model.pos

        ( w, h ) =
            model.size

        bonus =
            initbonus ( x + w / 2, y + h / 2 ) model.bonustype
    in
    if (model.valid == Killed) && not model.bonustaken then
        ( { model | bonustaken = True }, [ bonus ] )

    else
        ( model, [] )
