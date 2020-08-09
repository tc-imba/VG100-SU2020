module View exposing (..)


import Html exposing (audio , Html, button, div, text, h1, b, br, span)
import Html.Attributes exposing (style,class, controls, type_, src)
import Html.Events exposing (on, onClick, onMouseDown, onMouseUp)
import Json.Decode as Json
import Message exposing (Msg(..))
import Model exposing (Model, State(..))
import Svg exposing (path, rect, svg)
import Svg.Attributes exposing (width, height, viewBox, offset, stopColor, x1, x2, y1, y2, x, y, fill, id, d, stroke)
import Bricks
import Color exposing (..)
import Update

pixelWidth : Float
pixelWidth =
    1500


pixelHeight : Float
pixelHeight =
    550

view : Model -> Html Msg
view model =
    let
        ( w, h ) = model.size
        r = (min (w/pixelWidth) (h/pixelHeight))
    in
         div
            [ style "width" "100%"
            , style "height" "100%"
            , style "position" "absolute"
            , style "left" "0"
            , style "top" "0"
            , style "background-color" "#3d3c4b"
            ]
                [ div
                    [ style "width" (String.fromFloat pixelWidth ++ "px")
                    , style "height" (String.fromFloat pixelHeight ++ "px")
                    , style "position" "absolute"
                    , style "transform-origin" "0 0"
                    , style "transform" ("scale(" ++ String.fromFloat r ++ ")")
                    ]
                    [ div
                        []
                         [h1
                         [ style "margin-bottom" "0"
                         , style "text-align" "center"
                         , style "color" "#f3f2e9"
                         , style "margin" "30px"
                         ][text "Exocist: Haunted Gifts"]]
                    , div
                        [ style "height" (String.fromFloat pixelHeight ++ "px")
                        , style "width" "10%"
                        , style "float" "left"
                        ]
                         [ b []
                         [
                           renderStore model
                         , renderGameButton model
                         , renderStoreButton model
                         ]
                         ]
                    , renderResult model
                    , notebox model
                    , div
                        [ style "background-color" "#eadede"
                        , style "height" "450px"
                        , style "width" "810px"
                        , style "margin" "auto"
                        , style "border-radius" "20px"
                        , style "border-style" "inset"
                        , style "border-width" "20px"
                        ]
                        [renderPic model]
                    , div [ class "elm-audio-player"
                                           , style "position" "absolute"
                                           , style "left" "10px"
                                           , style "bottom" "-115px" ]
                                           [ audio [ src "./bgm.mp3"
                                                   , type_ "audio/mp3"
                                                   , controls True
                                                   ][]]
                    , div [style "clear" "both", style"text-align" "center", style "color" "#fff"]
                          [ text "AURORA"]
                    ]
                ]

renderResult: Model -> Html Msg
renderResult model =
    if (model.chance > 0) then
    div [
          style "border-style" "inset"
        , style "border-color" "white"
        , style "border-width" "6px"
        , style "border-radius" "20%"
        , style "width" "500px"
        , style "height" "20px"
        , style "background-color" "white"
        , style "position" "absolute"
        , style "left" "500px"
        , style "top" "600px"
        , style "text-align" "center"
        , style "margin" "auto"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "font-weight" "300"
          ]
           [ Html.text ( "Chance Remain: " )
           , div
                [ Html.Attributes.style "color" "red"
                , style "display" "inline"
                , style "padding-right" "2.5em"] [Html.text ( Debug.toString( model.chance ))]
           , Html.text ( "Your Score: " ++ Debug.toString ( model.score ) )


           ]
    else
     div [
           style "border-style" "inset"
         , style "border-color" "white"
         , style "border-width" "6px"
         , style "border-radius" "20%"
         , style "width" "500px"
         , style "height" "20px"
         , style "background-color" "white"
         , style "position" "absolute"
         , style "left" "500px"
         , style "top" "600px"
         , style "text-align" "center"
         , style "margin" "auto"
         , style "font-family" "Helvetica, Arial, sans-serif"
         , style "font-size" "18px"
         , style "font-weight" "300"
           ]

    [ Html.text "Start a New game!" ]






renderStore : Model -> Html Msg
renderStore model =
    case model.isStore of
        True ->
            div [     ]
            [button
            [ onClick BallLarger
            , Html.Attributes.style "width" "150px"
            , Html.Attributes.style "height" "50px"
            , Html.Attributes.style "font-size" "18px"
            , style "position" "absolute"
            , style "left" "95px"
            , style "top" "130px"
            ] [ text "larger (cost 5) "],

            button
            [ onClick BallSmaller
            , Html.Attributes.style "width" "150px"
            , Html.Attributes.style "height" "50px"
            , Html.Attributes.style "font-size" "18px"
            , style "position" "absolute"
            , style "left" "95px"
            , style "top" "200px"
            ] [ text "smaller (cost 5) "],



            button
            [ onClick BallSlower
            , Html.Attributes.style "width" "150px"
            , Html.Attributes.style "height" "50px"
            , Html.Attributes.style "font-size" "18px"
            , style "position" "absolute"
            , style "left" "95px"
            , style "top" "270px"
            ] [ text "slower (cost 5) "],

            button
            [ onClick BallFaster
            , Html.Attributes.style "width" "150px"
            , Html.Attributes.style "height" "50px"
            , Html.Attributes.style "font-size" "18px"
            , style "position" "absolute"
            , style "left" "95px"
            , style "top" "340px"
            ] [ text "faster (cost 5) "],

            button
            [ onClick GainChance
            , Html.Attributes.style "width" "150px"
            , Html.Attributes.style "height" "50px"
            , Html.Attributes.style "font-size" "18px"
            , style "position" "absolute"
            , style "left" "95px"
            , style "top" "410px"


            ] [ text "chance+1",br[][], text"(cost 10) "]



          ]
        _ -> div[][]



renderPic : Model -> Html Msg
renderPic model =
     let
         ballRenderer =
            case model.ball.attribute of
                Bricks.Humility -> Bricks.ball1tosvg
                Bricks.Kindness -> Bricks.ball2tosvg
                Bricks.Charity -> Bricks.ball3tosvg
                _ -> Bricks.ball4tosvg -- change the order by zhouyuxiang

     in
     svg
        [ width "810"
        , height "450"
        , viewBox "0 0 900 500"
        ] (
        [
        Svg.defs [] [ Svg.linearGradient [ Svg.Attributes.id "base1"
                                            , x1 "0", x2 "0", y1 "0", y2 "1"]
                                            [ Svg.stop [ offset "60%", stopColor color1 ] []
                                            , Svg.stop [ offset "100%", stopColor (toString(rgb 85 100 150)) ] []
                                            ]]
        , Svg.defs [] [ Svg.linearGradient [ Svg.Attributes.id "base2"
                                          , x1 "0", x2 "0", y1 "0", y2 "1"]
                                          [ Svg.stop [ offset "60%", stopColor color2 ] []
                                          , Svg.stop [ offset "100%", stopColor (toString(rgb 10 60 10)) ] []
                                          ]]
        , Svg.defs [] [ Svg.linearGradient [ Svg.Attributes.id "base3"
                                          , x1 "0", x2 "0", y1 "0", y2 "1"]
                                          [ Svg.stop [ offset "60%", stopColor color3 ] []
                                          , Svg.stop [ offset "100%", stopColor (toString(rgb 15 10 60)) ] []
                                          ]]
        , Svg.defs [] [ Svg.linearGradient [ Svg.Attributes.id "base4"
                                          , x1 "0", x2 "0", y1 "0", y2 "1"]
                                          [ Svg.stop [ offset "60%", stopColor color4 ] []
                                          , Svg.stop [ offset "100%", stopColor (toString(rgb 100 0 25)) ] []
                                          ]]
        , Svg.defs [] [ Svg.linearGradient [ Svg.Attributes.id "base5"
                                          , x1 "0", x2 "0", y1 "0", y2 "1"]
                                          [ Svg.stop [ offset "50%", stopColor color1weak ] []
                                          , Svg.stop [ offset "100%", stopColor (toString(rgb 85 100 150)) ] []
                                          ]]
        , Svg.defs [] [ Svg.linearGradient [ Svg.Attributes.id "base6"
                                          , x1 "0", x2 "0", y1 "0", y2 "1"]
                                          [ Svg.stop [ offset "50%", stopColor color2weak ] []
                                          , Svg.stop [ offset "100%", stopColor (toString(rgb 10 60 10)) ] []
                                          ]]
        , Svg.defs [] [ Svg.linearGradient [ Svg.Attributes.id "base7"
                                          , x1 "0", x2 "0", y1 "0", y2 "1"]
                                          [ Svg.stop [ offset "50%", stopColor color3weak ] []
                                          , Svg.stop [ offset "100%", stopColor (toString(rgb 15 10 60)) ] []
                                          ]]
        , Svg.defs [] [ Svg.linearGradient [ Svg.Attributes.id "base8"
                                          , x1 "0", x2 "0", y1 "0", y2 "1"]
                                          [ Svg.stop [ offset "50%", stopColor color4weak ] []
                                          , Svg.stop [ offset "100%", stopColor (toString(rgb 100 0 25)) ] []
                                          ]]
        , Svg.defs [] [ Svg.radialGradient [ Svg.Attributes.id "ball1"
                                          , x1 "0", x2 "0", y1 "0", y2 "1"]
                                          [ Svg.stop [ offset "50%", stopColor color_ball1 ] []
                                          , Svg.stop [ offset "100%", stopColor background ] []
                                          ]]
        , Svg.defs [] [ Svg.radialGradient [ Svg.Attributes.id "ball2"
                                          , x1 "0", x2 "0", y1 "0", y2 "1"]
                                          [ Svg.stop [ offset "50%", stopColor color_ball2 ] []
                                          , Svg.stop [ offset "100%", stopColor background ] []
                                          ]]
        , Svg.defs [] [ Svg.radialGradient [ Svg.Attributes.id "ball3"
                                          , x1 "0", x2 "0", y1 "0", y2 "1"]
                                          [ Svg.stop [ offset "50%", stopColor color_ball3 ] []
                                          , Svg.stop [ offset "100%", stopColor background ] []
                                          ]]
        , Svg.defs [] [ Svg.radialGradient [ Svg.Attributes.id "ball4"
                                          , x1 "0", x2 "0", y1 "0", y2 "1"]
                                          [ Svg.stop [ offset "50%", stopColor color_ball4 ] []
                                          , Svg.stop [ offset "100%", stopColor background ] []
                                          ]]
        , Svg.defs [] [ Svg.linearGradient [ Svg.Attributes.id "wand"
                                           , x1 "0", x2 "0", y1 "0", y2 "1"]
                                           [ Svg.stop [ offset "40%", stopColor color_wand ] []
                                           , Svg.stop [ offset "120%", stopColor (toString (rgb 26 26 26))] []
                                           ]]
        , Svg.defs [] [ Svg.linearGradient [ Svg.Attributes.id "crystal"
                                           , x1 "0", x2 "1", y1 "0", y2 "0"]
                                           [ Svg.stop [ offset "10%", stopColor color_crys ] []
                                           , Svg.stop [ offset "120%", stopColor color_crys_lumi] []
                                           ]]
        , Svg.defs [] [ Svg.radialGradient [ Svg.Attributes.id "ring"
                                           , x1 "0", x2 "1", y1 "0", y2 "0"]
                                           [ Svg.stop [ offset "10%", stopColor color_ring ] []
                                           , Svg.stop [ offset "100%", stopColor background ] []
                                           ]]
        , Svg.mask [id "myMask", Svg.Attributes.maskContentUnits "objectBoundingBox"]
                   [rect    [fill "white", x "0", y "0", width "100%", height "100%" ][]
                   ,Svg.polygon [fill color_crys_lumi, Svg.Attributes.points "0.4,0 0.4,1 0.6,1 0.6,0"][] ]
        , Svg.mask [id "myMask2", Svg.Attributes.maskContentUnits "objectBoundingBox"]
                   [rect    [fill "white", x "0", y "0", width "100%", height "100%" ][]
                   ,Svg.polygon [fill color_crys_lumi, Svg.Attributes.points "0,0.4 1,0.4 1,0.6 0,0.6"][] ]
        , Svg.image [Svg.Attributes.xlinkHref "./geograph-2430824-by-Adrian-Cable.jpg"
                   , x "0"
                   , y "0"
                   , width "1000"
                   , height "700"
                   , Svg.Attributes.transform "translate(-50,-50)"][]
        , rect [ x "0"
               , y "0"
               , width "900"
               , height "200"
               , fill (toString(rgb 255 255 255))
               , Svg.Attributes.opacity "0.3"] []
        ]
        ++

        ( Bricks.tosvg model.bricks
        ++ Bricks.toptosvg_ model.bricks
        ++ Bricks.tietosvg_ model.bricks
        ++ Bricks.boardtosvg model.board
        ++ ballRenderer model.ball)
         )


renderStoreButton : Model -> Html Msg
renderStoreButton model =
    let
        top_str =
            case model.isStore of
                True -> "507px"
                _ -> "130px"

    in
    button
        [ Html.Attributes.style "background" "#444343"
        , style "position" "absolute"
        , style "left" "105px"
        , style "top" top_str
        --, Html.Attributes.style "bottom" "30px"
        , Html.Attributes.style "color" "#f3f2e9"
        , Html.Attributes.style "cursor" "pointer"
        , Html.Attributes.style "display" "block"
        , Html.Attributes.style "font-family" "Helvetica, Arial, sans-serif"
        , Html.Attributes.style "font-size" "18px"
        , Html.Attributes.style "font-weight" "300"
        , Html.Attributes.style "height" "80px"
        , Html.Attributes.style "line-height" "60px"
        , Html.Attributes.style "outline" "none"
        , Html.Attributes.style "padding" "0"
        , Html.Attributes.style "width" "130px"
        --, style "margin" "30px"
        , style "border-style" "inset"
        , style "border-color" "white"
        , style "border-width" "6px"
        , style "border-radius" "20%"
        , onClick ShowStore
        ]
        [ Html.text "Store" ]


renderGameButton : Model -> Html Msg --changed to model by zhouyuxiang
renderGameButton model =
    let
        ( txt, msg ) =
            case model.state of
                Model.Stopped ->
                    ( "New game", Start )

                Model.Playing ->
                    ( "Pause", Pause )

                Model.Paused ->
                    ( "Resume", Resume )

                Model.End ->
                    ( "Restart", Restart )

                Model.Win ->
                    ( "Boss Room" , GoWrath )

        top_str =
            case model.state of
                Paused -> "505px"
                _ -> "130px"
    in
    button
        [ Html.Attributes.style "background" "#444343"
        , style "position" "absolute"
        , style "left" "1265px"
        , style "top" top_str
        --, Html.Attributes.style "bottom" "30px"
        , Html.Attributes.style "color" "#f3f2e9"
        , Html.Attributes.style "cursor" "pointer"
        , Html.Attributes.style "display" "block"
        , Html.Attributes.style "font-family" "Helvetica, Arial, sans-serif"
        , Html.Attributes.style "font-size" "18px"
        , Html.Attributes.style "font-weight" "300"
        , Html.Attributes.style "height" "80px"
        , Html.Attributes.style "line-height" "60px"
        , Html.Attributes.style "outline" "none"
        , Html.Attributes.style "padding" "0"
        , Html.Attributes.style "width" "130px"
        --, style "margin" "30px"
        , style "border-style" "inset"
        , style "border-color" "white"
        , style "border-width" "6px"
        , style "border-radius" "20%"
        , onClick msg
        ]
        [ Html.text txt ]

notebox model =  -- To show the restraint rule (as img)
    case model.state of
        Paused ->
            div [
                ] [ Html.img [Html.Attributes.src "./Instruction.PNG"
                     , width "260"
                     , height "370"
                     , style "opacity" "1"
                     , style "position" "absolute"
                     , style "left" "1200px"
                     , style "top" "100px"
                     , style "border-style" "inset"
                     , style "border-color" "white"
                     , style "border-width" "6px"
                     , style "border-radius" "5%"
                     , Html.Attributes.alt "humility(white) -> pride(grey); kindness(green) -> envy(pink); charity(blue) -> greed(purple); patience(orange) -> wrath(red); if you don't use the restraint, you'll hit the gifts twice to purify them."][]
                    ]

        _ -> div[][]