module Subscriptions exposing (subscriptions, keyMapping)

import Model exposing (Model)
import Update exposing (Msg(..), Key(..))

import Json.Decode as Decode
import Browser.Events

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta OnFrame
        , Browser.Events.onResize (\w h -> GetSize w h)
        , Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
        , Browser.Events.onKeyUp (Decode.map KeyUp keyDecoder)
        , Browser.Events.onClick (Decode.succeed Click)
        ]

keyDecoder : Decode.Decoder Key
keyDecoder = 
    Decode.field "key" Decode.string
        |> Decode.andThen keyMapping
        
keyMapping : String -> Decode.Decoder Key
keyMapping raw = 
    case raw of
        "ArrowLeft" ->
            Decode.succeed (Move -1)
        "ArrowRight" ->
            Decode.succeed (Move 1)
        "ArrowUp" ->
            Decode.succeed Attack
        " " ->
            Decode.succeed Launch
        "E" ->
            Decode.succeed ( EasterEgg "E" )
        "X" ->
            Decode.succeed ( EasterEgg "X" )
        "P" ->
            Decode.succeed ( EasterEgg "P" )
        "D" ->
            Decode.succeed ( EasterEgg "D" )
        "H" ->
            Decode.succeed ( EasterEgg "H" )
        "I" ->
            Decode.succeed ( EasterEgg "I" )
        "L" ->
            Decode.succeed ( EasterEgg "L" )
        "W" ->
            Decode.succeed ( EasterEgg "W" )  
        "T" ->
            Decode.succeed ( EasterEgg "T" )
        "O" ->
            Decode.succeed ( EasterEgg "O" )
        "N" ->
            Decode.succeed ( EasterEgg "N" )
        "R" ->
            Decode.succeed ( EasterEgg "R" )
        "A" ->
            Decode.succeed ( EasterEgg "A" )
        "Y" ->
            Decode.succeed ( EasterEgg "Y" )
        _ ->
            Decode.succeed Null
