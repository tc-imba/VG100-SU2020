module View exposing (..)
import Game exposing (Country(..), GameModel, Status(..), n_)
import Message exposing (..)
import Model as Music exposing (Model(..), Music)
import Shape exposing (..)
import Html exposing (Html, audio, button, div, p, text)
import Html.Attributes exposing (autoplay, controls, loop, src, style)
import Html.Events exposing (onClick)
import Svg exposing (svg)
import Svg.Attributes
import List exposing (..)



view : Model -> Html Msg
view model =
    case model of
       Start help->
           startview model help

       Prepare con music conm->
           prepareview model con music conm

       Game gamemodel music->
           gameview gamemodel music

       End music->
           endview model music


startview : Model -> Bool -> Html Msg
startview model help=
    let
        opa =
            if help then
                "0.6"
            else
                "0"
        opan =
            if help then
                "1"
            else
                "0"

        new = not help


    in
    div
    [  style "width" "100%"
     , style "height" "100%"
     , style "left" "0"
     , style "top" "0"
     , style "background" "url(picture/startnew.jpg) 0% 0% / 100% 100%"
     , style "background-repeat" "no-repeat"
     , style "position" "absolute"

    ]
    [ div
       [  style "height" "35%"
        , style "width" "50%"
        , style "top" "32%"
        , style "left" "25%"
        , style "background" "#FFF"
        , style "position" "absolute"
        , style "font-family" "Arial"
        , style "font-size" "20px"
        , style "color" "#FFF"
        , style "text-align" "center"
        , style "opacity" opa
       ]
       []
     , div
       [  style "height" "35%"
        , style "width" "50%"
        , style "top" "32%"
        , style "left" "25%"
        , style "position" "absolute"
        , style "color" "#000"
        , style "font-family" "Arial"
        , style "font-size" "24px"
        , style "overflow-x" "hide"
        , style "overflow" "auto"
        , style "opacity" opan
       ]
       [  p [style "text-align" "center" , style "font-size" "28px"] [text "Welcome to the game CONQUER. "]
        , p [] [text "In this game, you need to choose one of the three empires: Roman, Visigoth and Persian."]
        , p [] [text "First, press 'start' button to enter the country selection."]
        , p [] [text "Then, press the flags of the three countries to see its description."]
        , p [] [text "Choose one country and start the game."]
        , p [] [text "Use '←' and '→' on your keyboard to move the board and bounce the ball to fight with the enemies (the blocks)."]
        , p [] [text "Remember different enemies have different effects."]
        , p [] [text "The effects are as follows:"]
        , p [style "color" "#6D077D"] [text "Purple block: Kill certain enemies and turn itself into stone explosive."]
        , p [style "color" "#cd1e1e"] [text "Red block: Heal three random enemies."]
        , p [style "color" "#225cbf"] [text "Blue block: Change color of blocks, no effect on explosive."]
        , p [style "color" "#1f7f04"] [text "Green blocks: no special effects."]
        , p [] [text "The ball of different countries have different speed."]
        , p [] [text "Different countries have different skills, it can be triggered by accumulating the progressing bar."]
        , p [] [text "The skills are as follows:"]
        , p [] [text "Rome: the ball will once function as a bomb and hit a block and randomly explode some blocks around it"]
        , p [] [text "Persian: For some time, the length of the board will double and the speed of the ball will be 1.5 times faster."]
        , p [] [text "Visigoth: Get one more ball and after all the balls are dropped, you will lose."]
        , p [] [text "May God with you!"]
       ]
     , button
       [  style "height" "12%"
        , style "width" "18%"
        , style "bottom" "16%"
        , style "left" "22%"
        , style "border" "none"
        , style "color" "#030000"
        , style "position" "absolute"
        , style "opacity" "0"
        , style "cursor" "pointer"
        , onClick Startgame
       ]
       []
     , button
       [  style "height" "12%"
        , style "width" "14%"
        , style "bottom" "16%"
        , style "left" "63%"
        , style "background-color" "#470f0f"
        , style "position" "absolute"
        , style "border" "none"
        , style "color" "#eea140"
        , style "opacity" "0"
        , style "cursor" "pointer"
        , onClick (Help new)
       ]
       []
     , div
       [  style "top" "0%"
        , style "left" "0%"
        , style "height" "8%"
        , style "width" "4.5%"
        , style "background" "url(picture/logonew.jpg) 0% 0% / 100% 100%"
        , style "background-repeat" "no-repeat"
        , style "border" "none"
        , style "position" "absolute"

       ]
       []

    ]


prepareview : Model -> Game.Country -> Music -> Music -> Html Msg
prepareview model con music conm =
    let
        opaRome =
            if con == Game.Rome then
                "1"
            else
                "0.6"
        opaGoth =
            if con == Game.Goth then
                "1"
            else
                "0.6"
        opaPersia =
            if con == Game.Persia then
                "1"
            else
                "0.6"
        play =
            case music of
                Music.Play ->
                    audio
                            [  autoplay True
                             , controls False
                             , src "audio/prepare.ogg"
                             , style "type" "audio/ogg"
                             , loop True
                            ]
                            [ ]
                _ ->
                    div [] []

        conmu =
            case conm of
                Music.Rome ->
                    audio
                            [ autoplay True
                            , controls False
                            , src "audio/rome.ogg"
                            , style "type" "audio/ogg"
                            ]
                            [ ]
                Music.Persia ->
                    audio
                            [ autoplay True
                            , controls False
                            , src "audio/persia.ogg"
                            , style "type" "audio/ogg"
                            ]
                            [ ]
                Music.Visigoth ->
                    audio
                            [ autoplay True
                            , controls False
                            , src "audio/visigoth.ogg"
                            , style "type" "audio/ogg"
                            ]
                            [ ]
                _ ->
                    div [] []
    in

    div
        [  style "width" "100%"
         , style "height" "100%"
         , style "left" "0"
         , style "top" "0"
         , style "background" "url(picture/preparenew.jpg) 0% 0% / 100% 100%"
         , style "background-repeat" "no-repeat"
         , style "position" "absolute"
        ]
        [  button
           [  style "height" "10%"
            , style "width" "10%"
            , style "bottom" "15%"
            , style "left" "18%"
            , style "background" "url(picture/Rome.jpg) 0% 0% / 100% 100%"
            , style "background-repeat" "no-repeat"
            , style "border" "none"
            , style "position" "absolute"
            , style "cursor" "pointer"
            , style "opacity" opaRome
            , onClick CRome
           ]
           [ ]
         , button
           [  style "height" "10%"
            , style "width" "10%"
            , style "bottom" "15%"
            , style "left" "45%"
            , style "background" "url(picture/Persia.jpg)  0% 0% / 100% 100%"
            , style "background-repeat" "no-repeat"
            , style "border" "none"
            , style "position" "absolute"
            , style "cursor" "pointer"
            , style "opacity" opaPersia
            , onClick CPersia
            ]
            []
         , button
           [  style "height" "10%"
            , style "width" "10%"
            , style "bottom" "15%"
            , style "left" "73%"
            , style "background" "url(picture/Goth.jpg)  0% 0% / 100% 100%"
            , style "background-repeat" "no-repeat"
            , style "border" "none"
            , style "position" "absolute"
            , style "cursor" "pointer"
            , style "opacity" opaGoth
            , onClick CGoth
            ]
            []
         , div
           [  style "top" "0%"
            , style "left" "0%"
            , style "height" "8%"
            , style "width" "4.5%"
            , style "background" "url(picture/logonew.jpg) 0% 0% / 100% 100%"
            , style "background-repeat" "no-repeat"
            , style "border" "none"
            , style "position" "absolute"

           ]
           []
         , description con
         , goButton con
         , play
         , conmu
        ]


description : Country -> Html Msg
description con =
    let
        title =
            case con of
                Rome ->
                    "ROME    level:Monkey"

                Persia ->
                    "PERSIA    level:Normal"

                Goth ->
                    "VISIGOTH    level:VG100"

                _ ->
                    ""
        skill =
            case con of
                Rome ->
                    "Smash (Attack Skill): Your next attack will kill the enemy and random enemies around it."
                Persia ->
                    "Bulk up (Defense Skill): You will have a stronger board for the duration."
                Goth ->
                    "Double team (Survival Skill): You will get one more weapon (ball)."
                _ -> ""

        txt =
            case con of
                Rome ->
                    "The Eastern Roman Empire, also referred to as the Byzantine Empire. Its capital city was Constantinople. During most of its existence, the empire was the most powerful economic, cultural, and military force in Europe. About the sixth century AD, the empire reached its greatest extent, after reconquering much of the historically Roman western Mediterranean coast, including North Africa, Italy and Rome, which it held for two more centuries."

                Persia ->
                    "The Sasanian Empire, endured for over four centuries, from 224 to 651 AD, making it the longest-lived Persian dynasty. The Sasanian Empire succeeded the Parthian Empire, and reestablished the Iranians as a superpower.  It is recognized as one of the leading world powers in late antiquity, alongside its neighbouring arch-rival, the Roman-Byzantine Empire."

                Goth ->
                    "The Visigoths were an early Germanic people who along with the Ostrogoths constituted the two major political entities of the Goths within the Roman Empire in Late Antiquity. The Visigoths emerged from earlier Gothic groups, who had moved into the Roman Empire beginning in 376 and had played a major role in defeating the Romans at the Battle of Adrianople in 378."

                No ->
                    ""

        opa_ =
            case con of
                No ->
                    "0"
                _ ->
                    "1"

        opa =
            case con of
                No ->
                    "0"

                _ ->
                    "0.6"
    in
        div
        [  style "height" "32%"
         , style "width" "50%"
         , style "top" "28%"
         , style "left" "25%"
         , style "position" "absolute"
         , style "text-align" "center"
         , style "font-family" "Arial"
         , style "font-size" "24px"
         , style "color" "#eeedeb"
        ]
        [ div
             [ style "height" "100%"
             , style "width" "100%"
             , style "position" "absolute"
             , style "left" "0"
             , style "top" "0"
             , style "background" "#FFF"
             , style "opacity" opa
             ]
             []
         , div [ style "height" "100%"
              , style "width" "100%"
              , style "position" "absolute"
              , style "left" "0"
              , style "top" "0"
              , style "text-align" "center"
              , style "font-family" "Arial"
              , style "font-size" "24px"
              , style "overflow-x" "hide"
              , style "overflow" "auto"
              , style "opacity" opa_
              ]
                [ p [  style "text-align" "center"
                     , style "font-family" "Arial"
                     , style "font-size" "32px"
                     , style "color" "#000"
                    ] [text title]
                , p [ style "text-align" "left"
                    , style "font-family" "Arial"
                    , style "font-size" "28px"
                    , style "color" "#000"
                    ] [text skill]
                , p [ style "font-family" "Arial"
                    , style "text-align" "justify"
                    , style "font-size" "24px"
                    , style "color" "#000"
                    ] [text txt]
                ]

        ]


goButton : Country -> Html Msg
goButton con =
    let
        msg =
            case con of
                Rome ->
                    Go Rome

                Persia ->
                    Go Persia

                Goth ->
                    Go Goth

                _ ->
                    Startgame
        opa =
            case con of
                No ->
                    "0"

                _ ->
                    "1"
        eve =
            case con of
                No ->
                    "none"

                _ ->
                    "auto"
    in
        button
        [  style "top" "40%"
         , style "right" "8%"
         , style "height" "110px"
         , style "width" "200px"
         , style "background" "url(picture/go.png)  0% 0% / 100% 100%"
         , style "background-repeat" "no-repeat"
         , style "border" "none"
         , style "color" "#ffffff"
         , style "position" "absolute"
         , style "font-size" "30px"
         , style "font-weight" "900"
         , style "cursor" "pointer"
         , style "opacity" opa
         , style "pointer-events" eve
         , onClick msg]
        []


endview : Model -> Music -> Html Msg
endview model music =
    let
        play =
            case music of
                 Music.Win ->
                     audio
                         [  autoplay True
                          , controls False
                          , src "audio/end.ogg"
                          , style "type" "audio/ogg"
                          , loop True
                         ]
                         [ ]
                 _ ->
                     div [] []
    in

    div
    [  style "width" "100%"
     , style "height" "100%"
     , style "left" "0"
     , style "top" "0"
     , style "background" "url(picture/endnew.jpg) 0% 0% / 100% 100%"
     , style "background-repeat" "no-repeat"
     --, style "background-size" "cover"
     , style "position" "absolute"
   ]
   [  button
      [  style "height" "8%"
       , style "width" "4.5%"
       , style "top" "0%"
       , style "left" "0%"
       , style "background" "url(picture/logonew.jpg) 0% 0% / 100% 100%"
       , style "background-repeat" "no-repeat"
       , style "border" "none"
       , style "position" "absolute"
      ]
      []
    , play
    , button [ style "height" "10%"
             , style "width" "20%"
             , style "position" "absolute"
             , style "left" "40%"
             , style "bottom" "10%"
             , style "background" "url(picture/restart.png) 0% 0% / 100% 100%"
             , style "cursor" "pointer"
             , onClick Restart
             ] []

   ]


gameview : GameModel -> Music -> Html Msg
gameview gamemodel music =
    let
        help =
            case (gamemodel.sta, gamemodel.tur) of
                (Win, 1) ->
                    div
                    [  style "height" "100%"
                     , style "width" "100%"
                     , style "position" "absolute"
                     , style "left" "0"
                     , style "top" "0"
                     , style "color" "#d90c0c"
                     , style "font-family" "Arial"
                     , style "font-size" "24px"
                    ]
                    [  div [ style "height" "100%"
                           , style "width" "100%"
                           , style "position" "absolute"
                           , style "left" "0"
                           , style "top" "0"
                           , style "background" "#fff"
                           , style "opacity" "0.6"
                           ]
                           []
                     , div [   style "height" "50%"
                             , style "width" "100%"
                             , style "position" "absolute"
                             , style "left" "0"
                             , style "top" "25%"
                             , style "color" "#d90c0c"
                             , style "font-family" "Arial"
                             , style "font-size" "24px"
                             , style "text-align" "center"
                       ]
                       [ p[] [text "Congratulations on defeating your first enemy!"]
                       , p[] [text "Soon you will face your second enemy."]
                       , p[] [text "They sent in more elite troops."]
                       , p[] [text "You have to hit it twice to beat the green brick."]
                       , p[] [text "God with you!"]
                       ]
                    ]
                _ ->
                    div[][]

        music_ =
            case gamemodel.sta of
                Win ->
                    Music.Win

                GG ->
                    Music.Lose

                _ ->
                    music

        play =
            case music_ of
                Music.Play ->
                    audio
                            [ autoplay True
                            , controls False
                            , src "audio/game.ogg"
                            , style "type" "audio/ogg"
                            , loop True
                            ]
                            [ ]

                Music.Win ->
                    audio
                            [ autoplay True
                            , controls False
                            , src "audio/win.ogg"
                            , style "type" "audio/ogg"
                            , loop True
                            ]
                            [ ]

                Music.Lose ->
                    audio
                            [ autoplay True
                            , controls False
                            , src "audio/lose.ogg"
                            , style "type" "audio/ogg"
                            , loop True
                            ]
                            [ ]

                _ ->
                    div [] []
    in

    div
    [  style "width" "100%"
     , style "height" "100%"
     , style "left" "0"
     , style "top" "0"
     , style "background" "url(picture/gamenew.jpg) 0% 0% / 100% 100%"
     , style "background-repeat" "no-repeat"
     --, style "background-size" "cover"
     , style "position" "absolute"
     ]
     [  button
        [  style "height" "8%"
         , style "width" "4.5%"
         , style "top" "0%"
         , style "left" "0%"
         , style "background" "url(picture/logonew.jpg) 0% 0% / 100% 100%"
         , style "background-repeat" "no-repeat"
         , style "border" "none"
         , style "position" "absolute"
         , style "cursor" "pointer"
        ]
        []
      , div
        [   style "height" "95%"
          , style "width" "50%"
          , style "left" "25%"
          , style "top" "10px"
          , style "position" "absolute"
          , style "background" "#fff"
          , style "opacity" "0.5"
        ] []
      , div
        [  style "height" "95%"
         , style "width" "50%"
         , style "left" "25%"
         , style "top" "10px"
         , style "position" "absolute"
         , style "outline-style" "solid"
         , style "outline-width" "5px"
        ]
        [  renderAll gamemodel , help]
         , renderRightPanel gamemodel
         , renderLeftPanel gamemodel


         , play
     ]


backButton : Game.Status -> Html Msg
backButton sta =
    let
        opa =
            case sta of
                GG  ->
                    "1"

                Empty ->
                    "1"

                _ ->
                    "0"
        eve =
            case sta of
                GG ->
                    "auto"

                Empty ->
                    "auto"

                _ ->
                    "none"
    in
        button
        [  style "height" "7%"
         , style "width" "20%"
         , style "top" "85%"
         , style "right" "40%"
         , style "background" "url(picture/back.png) 0% 0% / 100% 100%"
         , style "background-repeat" "no-repeat"
         , style "position" "absolute"
         , style "border" "none"
         , style "color" "#ffffff"
         , style "font-size" "23px"
         , style "font-weight" "600"
         , style "cursor" "pointer"
         , style "opacity" opa
         , style "pointer-events" eve
         , onClick Startgame
        ]
        []

renderAll : GameModel -> Html Msg
renderAll gamemodel =
     svg
        [ Svg.Attributes.width "100%"
        , Svg.Attributes.height "100%"
        , Svg.Attributes.style " "
        , Svg.Attributes.viewBox  ( "-225 -300 " ++ String.fromInt(n_.wid) ++ " " ++ String.fromInt(n_.wid) )
        , Svg.Attributes.preserveAspectRatio("none")
        ]
        (List.map svgShape (shapeAll gamemodel))


renderRightPanel : GameModel -> Html Msg
renderRightPanel gamemodel =
    div
        [  style "height" "100%"
         , style "width" "25%"
         , style "right" "0"
         , style "top" "0"
         , style "color" "#af2323"
         , style "position" "absolute"
        ]
        [  div
           [  style "width" "100%"
            , style "height" "15%"
            , style "top" "8%"
            , style "left" "0%"
            , style "color" "#AC0B0B"
            , style "font-size" "50px"
            , style "text-align" "center"
            , style "position" "absolute"
           ]
           [text "Enemy"]
         , showFlag gamemodel.ene
         , renderNum gamemodel gamemodel.lis
         , backButton gamemodel.sta
         , renderGameButton gamemodel.sta gamemodel.tur
        ]


renderLeftPanel : GameModel -> Html Msg
renderLeftPanel gamemodel =
    div
        [  style "height" "100%"
         , style "width" "25%"
         , style "left" "0"
         , style "top" "0"
         , style "color" "#af2323"
         , style "position" "absolute"
        ]
        [  div
           [  style "width" "100%"
            , style "height" "15%"
            , style "top" "8%"
            , style "left" "0%"
            , style "color" "#AC0B0B"
            , style "font-size" "50px"
            , style "text-align" "center"
            , style "position" "absolute"
           ]
           [text "You"]
        , showFlag gamemodel.con
        -- , renderState gamemodel.sta gamemodel.tur
        , showSkill gamemodel
        ]

showSkill : GameModel -> Html Msg
showSkill model =
    let
        wid =
            String.fromFloat (model.ski / 100 * 80) ++ "%"

        pct =
            if (modBy 10 (floor(model.ski * 100))) == 0 then
                if (modBy 100 (floor(model.ski * 100))) == 0 then
                    String.fromFloat ((toFloat (floor(model.ski * 100))) / 100) ++ ".00%/100%"
                else
                    String.fromFloat ((toFloat (floor(model.ski * 100))) / 100) ++ "0%/100%"
            else
                String.fromFloat ((toFloat (floor(model.ski * 100))) / 100) ++ "%/100%"
        r =
            if model.ski < 40 then
                model.ski * 6.375
            else
                255
        g =
            if model.ski < 40 then
                255
            else if model.ski >= 40 && model.ski <= 80 then
                255 - (model.ski - 40) * 6.375
            else
                0
        skillname =
            case model.stp of
                1 -> "Double Team"
                2 -> "Smash"
                3 -> "Bulk Up"
                _ -> ""

        color =
            "rgb(" ++ (String.fromFloat r) ++ "," ++ (String.fromFloat g) ++ ",0)"

    in
    div [ style "width" "100%"
        , style "height" "30%"
        , style "position" "absolute"
        , style "left" "0"
        , style "top" "50%"
        ]
        [ div [ style "width" "100%"
              , style "height" "33%"
              , style "text-align" "center"
              , style "font-size" "40px"
              , style "color" "#fff"
              ]
              [ text skillname]

        , div [style "width" "80%"
             , style "height" "15%"
             , style "position" "absolute"
             , style "top" "53%"
             , style "left" "10%"
             , style "background" "#fff"
             , style "opacity" "0.6"
             ] []

        , div [ style "width" wid
              , style "height" "15%"
              , style "position" "absolute"
              , style "top" "53%"
              , style "left" "10%"
              , style "background" color
              ]
              [
              ]

        , div [  style "width" "80%"
               , style "height" "15%"
               , style "position" "absolute"
               , style "top" "53%"
               , style "left" "10%"
               , style "color" "#000"
               , style "text-align" "center"
               , style "font-size" "24px"
              ]
              [ text pct]
        ]


showFlag : Country -> Html Msg
showFlag con =
    let
        url =
            case con of
                Rome ->
                    "url(picture/Rome.jpg) 0% 0% / 100% 100%"

                Persia ->
                    "url(picture/Persia.jpg) 0% 0% / 100% 100%"

                Goth ->
                    "url(picture/Goth.jpg) 0% 0% / 100% 100%"

                _ ->
                    ""
    in

    div
    [  style "width" "80%"
     , style "height" "20%"
     , style "left" "10%"
     , style "top" "20%"
     , style "position" "absolute"
     , style "background" url
     , style "background-repeat" "no-repeat"
    ]
    []


renderState : Game.Status -> Int -> Html Msg
renderState state turn =
    let
        txt =
            case state of
                Game.GG ->
                    [p [] [text "Defeated!"]
                    ,p [] [text "Try again."] ]

                Game.Playing ->
                    [p [] [text "FIGHT your Enemies!"]]

                Game.Paused ->
                    [p [] [text "Think of your statics!"]]

                Game.Win ->
                    if turn == 1 then
                        [p [] [text "You win your first battle!"]]
                    else
                        [p [] [text "You win your second battle!"]]

                Game.Empty ->
                    [p [] [text "Attack"]]

    in
        div
            [  style "position" "absolute"
             , style "width" "100%"
             , style "height" "20%"
             , style "top" "50%"
             , style "left" "0"
             , style "color" "#fff"
             , style "font-size" "50px"
             , style "text-align" "center"
            ]
            txt


renderGameButton : Game.Status -> Int -> Html Msg
renderGameButton state turn =
    let
        (txt, msg) =
            case (state, turn) of
                (Game.GG, 1) ->
                    ("url(picture/retry.png) 0% 0% / 100% 100%", Firstturn)

                (Game.GG, 2) ->
                    ("url(picture/retry.png) 0% 0% / 100% 100%", Secondturn)

                (Game.Playing, 1) ->
                    ("url(picture/pause.png) 0% 0% / 100% 100%", Pause)

                (Game.Playing, 2) ->
                    ("url(picture/pause.png) 0% 0% / 100% 100%", Pause)

                (Game.Paused, 1) ->
                    ("url(picture/resume.png) 0% 0% / 100% 100%", Resume)

                (Game.Paused, 2) ->
                    ("url(picture/resume.png) 0% 0% / 100% 100%", Resume)

                (Game.Win, 1) ->
                    ("url(picture/next.png) 0% 0% / 100% 100%", Secondturn)

                (Game.Win, 2) ->
                    ("url(picture/win.png) 0% 0% / 100% 100%", Gamewin)

                (Game.Empty ,1) ->
                    ("url(picture/fire.png) 0% 0% / 100% 100%" , Firstturn)

                (Game.Empty ,2) ->
                    ("url(picture/fire.png) 0% 0% / 100% 100%" , Secondturn)

                _ ->
                    ("", Noop)
    in

    button
        [  style "height" "10%"
         , style "width" "44%"
         , style "right" "28%"
         , style "bottom" "25%"
         , style "background" txt
         , style "background-repeat" "no-repeat"
         , style "position" "absolute"
         , style "border" "0"
         , style "color" "#ffffff"
         , style "cursor" "pointer"
         , style "display" "block"
         , style "font-family" "Helvetica, Arial, sans-serif"
         , style "font-size" "23px"
         , style "font-weight" "300"
         , style "line-height" "60px"
         , style "outline" "none"
         , style "padding" "0"
         , onClick msg
        ]
        []


renderNum : GameModel -> Game.Grid -> Html Msg
renderNum gamemodel grid =
    if gamemodel.sta == Game.Empty then
        text " "
    else
        let
            num = String.fromInt (List.length (filter (\x->x.sta/=Game.Vanished&&x.sta/=Game.Darken) grid))

        in
        div
            [  style "width" "100%"
             , style "height" "20%"
             , style "top" "50%"
             , style "left" "0"
             , style "color" "#f5e8e9"
             , style "font-size" "40px"
             , style "text-align" "center"
             , style "position" "absolute"
            ]
            [ text ("Enemies Left: " ++ num)]
