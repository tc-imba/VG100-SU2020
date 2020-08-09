module View exposing (..)

import Debug exposing (toString)
import Html exposing (Html, audio, button, div, img, text, textarea)
import Html.Attributes exposing (alt, autoplay, controls, disabled, loop, src, style)
import Html.Events exposing (onClick)
import String
import Svg exposing (Svg, image, rect)
import Svg.Attributes as Svg exposing (fillOpacity, height, preserveAspectRatio, rx, ry, stroke, strokeOpacity, strokeWidth, viewBox, width, x, xlinkHref, y)
import List exposing (..)

import Ball exposing (..)
import Paddle
import Fire
import Block exposing (Special(..), addSpecial)
import Model exposing (..)

--View
view : Model -> Html Msg
view model =
    dis model

--Organize all the Display<modelName> together
dis: Model -> Html Msg
dis model =
       case model.displayState of
           DisplayInit ->
                displayInit model
           DisplayChoice ->
                 displayChoice model
           DisplayTutorial ->
                 displayTutorial model
           DisplayLevelChoose ->
                 displayLevelChoose model
           DisplaySkillChoose ->
                 displaySkillChoose model
           DisplayReference ->
                 displayReference model
           DisplayGame ->
                 displayGame model
           DisplayTutorial2 ->
                 displayTutorial2 model

--Active the `WaitStart` state
checkStateToBegin: Model -> DisplayState -> Model
checkStateToBegin model displayState=
    case displayState of
        DisplayGame ->
            {model | displayState = displayState, state = WaitStart}
        _ ->
            {model | displayState = displayState}

--Initial GUI
displayInit : Model -> Html Msg
displayInit model =
    div [ style "width" "100%"
        , style "height" "100%"
        , style "left" "0%"
        , style "top" "0%"
        , style "background" "url(./display/init.jpg) 0% 0% / 100% 100% no-repeat"
        , style "position" "absolute"
        , style "z-index" "-1"
        ]
        [
        button [ onClick (ChangeDisplayState DisplayChoice)
               , style "box-shadow" "inset 27px 22px 26px 17px #ad1454"
               , style "background" "linear-gradient(to bottom, #a34b3e 5%, #de7062 100%)"
               , style "background-color" "#a34b3e"
               , style "border-radius" "42px"
               , style "display" "inline-block"
               , style "cursor" "pointer"
               , style "color" "#ffffff"
               , style "font-family" "Georgia"
               , style "font-size" "2vw"
               , style "font-weight" "bold"
               , style "font-style" "italic"
               , style "text-decoration" "none"
               , style "text-shadow" "0px -1px 0px #7a2a1d"
               , style "position" "absolute"
               , style "width" "15%"
               , style "height" "10%"
               , style "top" "40%"
               , style "left" "42.5%"
               ]
               [ text "Start" ],
        button [ onClick (ChangeDisplayState DisplayTutorial)
               , style "box-shadow" "inset 27px 22px 26px 17px #ad1454"
               , style "background" "linear-gradient(to bottom, #a34b3e 5%, #de7062 100%)"
               , style "background-color" "#a34b3e"
               , style "border-radius" "42px"
               , style "display" "inline-block"
               , style "cursor" "pointer"
               , style "color" "#ffffff"
               , style "font-family" "Georgia"
               , style "font-size" "2vw"
               , style "font-weight" "bold"
               , style "font-style" "italic"
               , style "text-decoration" "none"
               , style "text-shadow" "0px -1px 0px #7a2a1d"
               , style "position" "absolute"
               , style "width" "15%"
               , style "height" "10%"
               , style "top" "55%"
               , style "left" "42.5%"
               ]
               [ text "Tutorial" ],
        img [ style "left" "10%"
            , style "top" "5%"
            , style "width" "80%"
            , style "height" "20%"
            , style "position" "absolute"
            , src "./assets/title.png"
            , alt "Conqueror: 1453"
            ]
            [],
        button [ onClick (ChangeDisplayState DisplayReference)
               , style "box-shadow" "inset 27px 22px 26px 17px #ad1454"
               , style "background" "linear-gradient(to bottom, #a34b3e 5%, #de7062 100%)"
               , style "background-color" "#a34b3e"
               , style "border-radius" "42px"
               , style "display" "inline-block"
               , style "cursor" "pointer"
               , style "color" "#ffffff"
               , style "font-family" "Georgia"
               , style "font-size" "2vw"
               , style "font-weight" "bold"
               , style "font-style" "italic"
               , style "text-decoration" "none"
               , style "text-shadow" "0px -1px 0px #7a2a1d"
               , style "position" "absolute"
               , style "width" "15%"
               , style "height" "10%"
               , style "top" "70%"
               , style "left" "42.5%"
               ]
               [ text "About Us" ]
        ]

--GUI: Tutorial,page1
displayTutorial : Model -> Html Msg
displayTutorial model =
     div [ style "width" "100%"
         , style "height" "100%"
         , style "left" "0%"
         , style "top" "0%"
         , style "background" "url(./display/Tutorial1.jpg) 0% 0% / 100% 100% no-repeat"
         , style "position" "absolute"
         , style "z-index" "-1"
         ]
         [
         button [ onClick (ChangeDisplayState DisplayTutorial2)
                , style "box-shadowinset" "27px 22px 26px 17px #e6d02b"
                , style "background" "linear-gradient(to bottom, #ffec64 5%, #ffab23 100%)"
                , style "background-color" "#ffec64"
                , style "border-radius" "42px"
                , style "display" "inline-block"
                , style "cursor" "pointer"
                , style "color" "#333333"
                , style "font-family" "Georgia"
                , style "font-size" "2vw"
                , style "font-weight" "bold"
                , style "font-style" "italic"
                , style "text-decoration" "none"
                , style "text-shadow" "0px -1px 0px #ffee66"
                , style "position" "absolute"
                , style "width" "15%"
                , style "height" "10%"
                , style "left" "65%"
                , style "top" "20%"
                ]
                [ text "Next" ]
                ]

--GUI: Tutorial,page2
displayTutorial2 : Model -> Html Msg
displayTutorial2 model =
     div [ style "width" "100%"
         , style "height" "100%"
         , style "left" "0%"
         , style "top" "0%"
         , style "background" "url(./display/Tutorial2.jpg) 0% 0% / 100% 100% no-repeat"
         , style "position" "absolute"
         , style "z-index" "-1"
         ]
         [
         button [ onClick (ChangeDisplayState DisplayInit)
                , style "box-shadowinset" "27px 22px 26px 17px #e6d02b"
                , style "background" "linear-gradient(to bottom, #ffec64 5%, #ffab23 100%)"
                , style "background-color" "#ffec64"
                , style "border-radius" "42px"
                , style "display" "inline-block"
                , style "cursor" "pointer"
                , style "color" "#333333"
                , style "font-family" "Georgia"
                , style "font-size" "2vw"
                , style "font-weight" "bold"
                , style "font-style" "italic"
                , style "text-decoration" "none"
                , style "text-shadow" "0px -1px 0px #ffee66"
                , style "position" "absolute"
                , style "width" "15%"
                , style "height" "10%"
                , style "left" "65%"
                , style "top" "20%"
                ]
                [ text "Back" ]
                ]

--GUI: About Us
displayReference : Model -> Html Msg
displayReference model =
     div [ style "width" "100%"
         , style "height" "100%"
         , style "left" "0%"
         , style "top" "0%"
         , style "background" "url(./display/reference.jpg) 0% 0% / 100% 100% no-repeat"
         , style "position" "absolute"
         , style "z-index" "-1"
         ]
         [
         button [ onClick (ChangeDisplayState DisplayInit)
                , style "box-shadowinset" "27px 22px 26px 17px #e6d02b"
                , style "background" "linear-gradient(to bottom, #ffec64 5%, #ffab23 100%)"
                , style "background-color" "#ffec64"
                , style "border-radius" "42px"
                , style "display" "inline-block"
                , style "cursor" "pointer"
                , style "color" "#333333"
                , style "font-family" "Georgia"
                , style "font-size" "2vw"
                , style "font-weight" "bold"
                , style "font-style" "italic"
                , style "text-decoration" "none"
                , style "text-shadow" "0px -1px 0px #ffee66"
                , style "position" "absolute"
                , style "width" "15%"
                , style "height" "10%"
                , style "left" "50%"
                , style "top" "70%"
                ]
                [ text "Back" ]
                ]

--GUI: Start
displayChoice : Model -> Html Msg
displayChoice model =
    div [ style "width" "100%"
        , style "height" "100%"
        , style "left" "0%"
        , style "top" "0%"
        , style "background" "url(./display/choice.jpg) 0% 0% / 100% 100% no-repeat"
        , style "position" "absolute"
        , style "z-index" "-1"
        ]
        [
        img [ style "left" "10%"
            , style "top" "5%"
            , style "width" "80%"
            , style "height" "20%"
            , style "position" "absolute"
            , src "./assets/title.png"
            , alt "Conqueror: 1453"
            ]
            [],
        button [ onClick (ChangeDisplayState DisplayGame)
               , style "box-shadow" "inset 27px 22px 26px 17px #8a2a21"
               , style "background" "linear-gradient(to bottom, #c62d1f 5%, #f24437 100%)"
               , style "background-color" "#c62d1f"
               , style "border-radius" "42px"
               , style "display" "inline-block"
               , style "cursor" "pointer"
               , style "color" "#ffffff"
               , style "font-family" "Georgia"
               , style "font-size" "2vw"
               , style "font-weight" "bold"
               , style "font-style" "italic"
               , style "text-decoration" "none"
               , style "text-shadow" "0px -1px 0px #810e05"
               , style "position" "absolute"
               , style "width" "15%"
               , style "height" "10%"
               , style "left" "42.5%"
               , style "top" "32.5%"
               ]
               [ text "Play" ],
        button [ onClick (ChangeDisplayState DisplayLevelChoose)
               , style "box-shadow" "inset 27px 22px 26px 17px #8a2a21"
               , style "background" "linear-gradient(to bottom, #c62d1f 5%, #f24437 100%)"
               , style "background-color" "#c62d1f"
               , style "border-radius" "42px"
               , style "display" "inline-block"
               , style "cursor" "pointer"
               , style "color" "#ffffff"
               , style "font-family" "Georgia"
               , style "font-size" "2vw"
               , style "font-weight" "bold"
               , style "font-style" "italic"
               , style "text-decoration" "none"
               , style "text-shadow" "0px -1px 0px #810e05"
               , style "position" "absolute"
               , style "width" "15%"
               , style "height" "10%"
               , style "left" "42.5%"
               , style "top" "47.5%"
               ]
               [ text "Level" ],
        button [ onClick (ChangeDisplayState DisplaySkillChoose)
               , style "box-shadow" "inset 27px 22px 26px 17px #8a2a21"
               , style "background" "linear-gradient(to bottom, #c62d1f 5%, #f24437 100%)"
               , style "background-color" "#c62d1f"
               , style "border-radius" "42px"
               , style "display" "inline-block"
               , style "cursor" "pointer"
               , style "color" "#ffffff"
               , style "font-family" "Georgia"
               , style "font-size" "2vw"
               , style "font-weight" "bold"
               , style "font-style" "italic"
               , style "text-decoration" "none"
               , style "text-shadow" "0px -1px 0px #810e05"
               , style "position" "absolute"
               , style "width" "15%"
               , style "height" "10%"
               , style "left" "42.5%"
               , style "top" "62.5%"
               ]
               [ text "Skill" ],
        button [ onClick (ChangeDisplayState DisplayInit)
               , style "box-shadow" "inset 27px 22px 26px 17px #8a2a21"
               , style "background" "linear-gradient(to bottom, #c62d1f 5%, #f24437 100%)"
               , style "background-color" "#c62d1f"
               , style "border-radius" "42px"
               , style "display" "inline-block"
               , style "cursor" "pointer"
               , style "color" "#ffffff"
               , style "font-family" "Georgia"
               , style "font-size" "2vw"
               , style "font-weight" "bold"
               , style "font-style" "italic"
               , style "text-decoration" "none"
               , style "text-shadow" "0px -1px 0px #810e05"
               , style "position" "absolute"
               , style "width" "15%"
               , style "height" "10%"
               , style "left" "42.5%"
               , style "top" "77.5%"
               ]
               [ text "Back" ]
        ]


-- GUI: Start-> Skill
displaySkillChoose : Model -> Html Msg
displaySkillChoose model =
     div [ style "width" "100%"
         , style "height" "100%"
         , style "left" "0%"
         , style "top" "0%"
         , style "background" "url(./display/skill.jpg) 0% 0% / 100% 100% no-repeat"
         , style "position" "absolute"
         , style "z-index" "-1"
         ]

         [
         Svg.svg
             [ width "auto"
             , height "auto"
             , viewBox "-25 0 125 55"
             , preserveAspectRatio "none"
             ]
             ([image
                 [x "-20"
                 ,y "20%"
                 ,width "30"
                 , height "25"
                 ,xlinkHref (skillButton model)
                 ]
                 []]
                 ),
         button [ onClick (ChangeDisplayState DisplayChoice)
                , style "box-shadow" "inset 27px 22px 26px 17px #23395e"
                , style "background" "linear-gradient(to bottom, #2e466e 5%, #415989 100%)"
                , style "background-color" "#2e466e"
                , style "border-radius" "42px"
                , style "display" "inline-block"
                , style "cursor" "pointer"
                , style "color" "#ffffff"
                , style "font-family" "Georgia"
                , style "font-size" "2vw"
                , style "font-weight" "bold"
                , style "font-style" "italic"
                , style "text-decoration" "none"
                , style "text-shadow" "0px -1px 0px #263666"
                , style "position" "absolute"
                , style "width" "15%"
                , style "height" "10%"
                , style "top" "80%"
                , style "left" "42.5%"
                ]
                [ text "Back" ],
         button [ onClick (ChangeSkillShown FiveBall)
                , style "box-shadow" "inset 27px 22px 26px 17px #23395e"
                , style "background" "linear-gradient(to bottom, #2e466e 5%, #415989 100%)"
                , style "background-color" "#2e466e"
                , style "border-radius" "42px"
                , style "display" "inline-block"
                , style "cursor" "pointer"
                , style "color" "#ffffff"
                , style "font-family" "Georgia"
                , style "font-size" "2vw"
                , style "font-weight" "bold"
                , style "font-style" "italic"
                , style "text-decoration" "none"
                , style "text-shadow" "0px -1px 0px #263666"
                , style "position" "absolute"
                , style "width" "12%"
                , style "height" "10%"
                , style "top" "60%"
                , style "left" "30%"
                ]
                [ text "Mighty Cannon" ],
         button [ onClick (ChangeSkillShown LongPaddle)
                , style "box-shadow" "inset 27px 22px 26px 17px #23395e"
                , style "background" "linear-gradient(to bottom, #2e466e 5%, #415989 100%)"
                , style "background-color" "#2e466e"
                , style "border-radius" "42px"
                , style "display" "inline-block"
                , style "cursor" "pointer"
                , style "color" "#ffffff"
                , style "font-family" "Georgia"
                , style "font-size" "2vw"
                , style "font-weight" "bold"
                , style "font-style" "italic"
                , style "text-decoration" "none"
                , style "text-shadow" "0px -1px 0px #263666"
                , style "position" "absolute"
                , style "width" "12%"
                , style "height" "10%"
                , style "top" "60%"
                , style "left" "44%"
                ]
                [ text "Insurance" ],
         button [ onClick (ChangeSkillShown LongLife)
                , style "box-shadow" "inset 27px 22px 26px 17px #23395e"
                , style "background" "linear-gradient(to bottom, #2e466e 5%, #415989 100%)"
                , style "background-color" "#2e466e"
                , style "border-radius" "42px"
                , style "display" "inline-block"
                , style "cursor" "pointer"
                , style "color" "#ffffff"
                , style "font-family" "Georgia"
                , style "font-size" "2vw"
                , style "font-weight" "bold"
                , style "font-style" "italic"
                , style "text-decoration" "none"
                , style "text-shadow" "0px -1px 0px #263666"
                , style "position" "absolute"
                , style "width" "12%"
                , style "height" "10%"
                , style "top" "60%"
                , style "left" "58%"
                ]
                [ text "Revive" ],
                textarea [ style "width" "40%"
                         , style "height" "40%"
                         , style "color" "black"
                         , style "font-family" "Times New Roman"
                         , style "font-size" "1.7vw"
                         , style "resize" "none"
                         , style "position" "absolute"
                         , style "top" "15%"
                         , style "left" "30%"
                         , disabled True
                         ]
                         [ skillInformation model ]
                ]

-- Additional component of GUI: Start-> Skill
skillButton: Model -> String
skillButton model =

        case model.typeOfSkills of
            FiveBall ->
                 "./display/a.png"

            LongLife ->
                    "./display/d.png"

            LongPaddle ->
                   "./display/s.png"

-- Additional component of GUI: Start-> Skill
skillInformation: Model -> Html Msg
skillInformation model =
    case model.typeOfSkills of
        FiveBall ->
            text "The enemy has become weak and exhausted.\nSolider！Take advantage of this opportunity, use your wisdom, courage, and our army's most fierce cannon to completely destroy them!\n1.Press A to use the Skill. \n2.Effect: Fire five additional balls.\n3.Warning: Because this is an extreme waste of our military resources, this skill is only allowed to be used when there is just one ball on the battlefield."
        LongLife ->
            text "Ottoman soldiers are still constantly adding to the front.They vowed to seize this City of World's Desire.\n1.Press D to use the Skill.\n2.Effect: Lives +1."
        LongPaddle ->
            text "A large number of craftsmen were attracted by money and glory, serving in the Ottoman army.\nRelying on their wisdom and creativity, our cannons will not only be more powerful, but also be more safer.\n1.Press S to use the Skill. \n2.Effect: Width of Paddle +2.\n3.Warning: Subject to the technical level of our army, the maximum length of paddle is 17. (the initial length is 11)"

--GUI: Start-> Level
displayLevelChoose : Model -> Html Msg
displayLevelChoose model =
     let
         prompt =
             case model.difficulty of
                 Easy ->
                    ("Merhaba, new soldier! Hope you will survive in this long-drawn-out siege...", "#ad1453")
                 Medium ->
                    ("Cool down. The cannon ball becomes a little bit out of control, this is a go-big-or-go-home!", "#228B22")
                 Difficult ->
                    ("Veteran, if you're a skillful and calm enough, go and help Mehmed II to conquer Constantinople at any cost!", "#8B0000")
         prompt_2 =
             case model.difficulty of
                 Easy ->
                     "Easy:\nIn this mode, the speed of the cannon ball is relatively slow, and there's fewer possibilities that you'll be attacked by Greek Fire.\nThe scoring factor is 1 in this mode."
                 Medium ->
                     "Medium:\nIn this mode, the speed of the cannon ball is neither fast nor slow, and there's more possibilities that you'll be attacked by Greek Fire.\nThe scoring factor is 2 in this mode."
                 Difficult ->
                     "Difficult:\nIn this mode, your tactic and reaction will be tested.\nThe cannon ball is nearly out of control, and you'll be bombarded with Greek Fire.\nBe careful with the splitted balls, sometimes they will help you but sometimes distract you from making correct choice.\nThe scoring factor is 3 in this mode."
     in
         div [ style "width" "100%"
             , style "height" "100%"
             , style "left" "0%"
             , style "top" "0%"
             , style "background" "url(./display/level.jpg) 0% 0% / 100% 100% no-repeat"
             , style "position" "absolute"
             , style "z-index" "-1"
             ]
             [
             button [ onClick (ChangeDisplayState DisplayChoice)
                    , style "box-shadow" "inset 27px 22px 26px 17px #b02c00"
                    , style "background" "linear-gradient(to bottom, #d0451b 5%, #bf462e 100%)"
                    , style "background-color" "#d0451b"
                    , style "border-radius" "42px"
                    , style "display" "inline-block"
                    , style "cursor" "pointer"
                    , style "color" "#ffffff"
                    , style "font-family" "Georgia"
                    , style "font-size" "2vw"
                    , style "font-weight" "bold"
                    , style "font-style" "italic"
                    , style "text-decoration" "none"
                    , style "text-shadow" "0px -1px 0px #c26238"
                    , style "position" "absolute"
                    , style "width" "15%"
                    , style "height" "10%"
                    , style "top" "80%"
                    , style "left" "42.5%"
                    ]
                    [ text "Back" ],
             textarea [ style "width" "40%"
                      , style "height" "10%"
                      , style "color" (Tuple.second prompt)
                      , style "font-family" "Times New Roman"
                      , style "font-size" "1.5vw"
                      , style "resize" "none"
                      , style "position" "absolute"
                      , style "left" "30%"
                      , style "top" "65%"
                      , disabled True
                      ]
                      [ text (Tuple.first prompt) ],
             textarea [ style "width" "40%"
                      , style "height" "30%"
                      , style "color" (Tuple.second prompt)
                      , style "font-family" "Times New Roman"
                      , style "font-size" "1.5vw"
                      , style "resize" "none"
                      , style "position" "absolute"
                      , style "top" "15%"
                      , style "left" "30%"
                      , disabled True
                      ]
                      [ text prompt_2],
             button [ onClick EasyMsg
                    , style "box-shadow" "inset 27px 22px 26px 17px #b02c00"
                    , style "background" "linear-gradient(to bottom, #d0451b 5%, #bf462e 100%)"
                    , style "background-color" "#d0451b"
                    , style "border-radius" "42px"
                    , style "display" "inline-block"
                    , style "cursor" "pointer"
                    , style "color" "#ffffff"
                    , style "font-family" "Georgia"
                    , style "font-size" "2vw"
                    , style "font-weight" "bold"
                    , style "font-style" "italic"
                    , style "text-decoration" "none"
                    , style "text-shadow" "0px -1px 0px #c26238"
                    , style "position" "absolute"
                    , style "height" "10%"
                    , style "width" "12%"
                    , style "top" "50%"
                    , style "left" "30%"
                    ]
                    [ text "Easy" ],
             button [ onClick MediumMsg
                    , style "box-shadow" "inset 27px 22px 26px 17px #b02c00"
                    , style "background" "linear-gradient(to bottom, #d0451b 5%, #bf462e 100%)"
                    , style "background-color" "#d0451b"
                    , style "border-radius" "42px"
                    , style "display" "inline-block"
                    , style "cursor" "pointer"
                    , style "color" "#ffffff"
                    , style "font-family" "Georgia"
                    , style "font-size" "2vw"
                    , style "font-weight" "bold"
                    , style "font-style" "italic"
                    , style "text-decoration" "none"
                    , style "text-shadow" "0px -1px 0px #c26238"
                    , style "position" "absolute"
                    , style "height" "10%"
                    , style "width" "12%"
                    , style "top" "50%"
                    , style "left" "44%"
                    ]
                    [ text "Medium" ],
             button [ onClick DifficultMsg
                    , style "box-shadow" "inset 27px 22px 26px 17px #b02c00"
                    , style "background" "linear-gradient(to bottom, #d0451b 5%, #bf462e 100%)"
                    , style "background-color" "#d0451b"
                    , style "border-radius" "42px"
                    , style "display" "inline-block"
                    , style "cursor" "pointer"
                    , style "color" "#ffffff"
                    , style "font-family" "Georgia"
                    , style "font-size" "2vw"
                    , style "font-weight" "bold"
                    , style "font-style" "italic"
                    , style "text-decoration" "none"
                    , style "text-shadow" "0px -1px 0px #c26238"
                    , style "position" "absolute"
                    , style "height" "10%"
                    , style "width" "12%"
                    , style "top" "50%"
                    , style "left" "58%"
                    ]
                    [ text "Difficult" ]
             ]

--GUI: Start-> Play
displayGame : Model -> Html Msg
displayGame model =
    let
        playMusic =
            case model.state of
                Play ->
                    div []
                        [ audio [ src "./music/At The Gates of Constantinople.mp3", autoplay True, controls False, loop True ] []
                        ]
                Lose ->
                    div []
                        [ audio [ src "./music/lose.mp3", autoplay True, controls False, loop False ] []
                        ]
                Win ->
                    div []
                        [ audio [ src "./music/charge.mp3", autoplay True, controls False, loop False ] []
                        ]
                Pause ->
                    div []
                        [ audio [ src "./music/At The Gates of Constantinople.mp3", autoplay True, controls False, loop True ] []
                        ]
                LoseOneLife ->
                    div []
                        [ audio [ src "./music/At The Gates of Constantinople.mp3", autoplay True, controls False, loop True ] []
                        ]
                WaitNextStage ->
                    div []
                        [ audio [ src "./music/wallBreach.wav", autoplay True, controls False, loop False ] []
                        ]
                _ ->
                    div [] []
        sound : Ball -> Html msg
        sound ball =
            case ball.exist of
                True ->
                    case ball.ballState of
                        BlockToPaddle ->
                            div []
                            [ audio [ src "./music/bom.mp3", autoplay True, controls False, loop False] []
                            ]
                        _ ->
                            div [] []

                False ->
                    div [] []
        sound1 = sound (Tuple.first (oneBall model "b"))
        sound2 = sound (Tuple.first  (twoBall model "b"))
        prompt =
            case model.state of
                Pause ->
                    ("Time for a break, soldier! Press Enter to get back to battlefield! Hope you won't lose yourself in relaxation.", "#1453ad")
                Play ->
                    case model.player.lives of

                        2 ->
                            ("Don't panic, everything is still under control. Remember to use your special skills to destroy the wall!", "#d2691e")
                        1 ->
                            ("Be careful and concentrated to escape from Greek Fire, we are risking ourselves in your last single venture!", "#dc143c")
                        _ ->
                            ("We are gaining upper hand, my brave soldier! Keep firing and we'll break the gate!", "#ff0000")

                Win ->
                    ("Oh my brave soldier! We've conquered the city of desire at last, the world will remember us!", "#ff0000")
                Lose ->
                    ("We're cursed, we're cursed!!! We'd better withdraw now...", "#ad1453")
                WaitStart ->
                    ("The Byzantines are sheltering themselves from the impregnable gate! Fire the mighty Urban Cannon to show them!", "#ff4500")
                LoseOneLife ->
                    ("Be careful! The cannon balls hitting the ground are killing us!", "#808000")
                WaitNextStage ->
                    ("Hah! We're not so far from the final conquering. Press Enter to get into next round of siege!", "#808000")
                _ ->
                    ("", "#000000")
        difficultyPrompt =
            case model.difficulty of
                Easy ->
                    "./assets/easy.png"
                Medium ->
                    "./assets/medium.png"
                Difficult ->
                    "./assets/difficult.png"
        stagePrompt =
            case model.stage of
                1 ->
                    "./assets/stage1.png"
                2 ->
                    "./assets/stage2.png"
                3 ->
                    "./assets/stage3.png"
                4 ->
                    "./assets/stage4.png"
                5 ->
                    "./assets/stage5.png"
                6 ->
                    "./assets/stage6.png"
                _ ->
                    ""
        propmtImage =
            case model.state of
                Pause ->
                    "./assets/paused.png"
                Win ->
                    "./assets/win.png"
                Lose ->
                    "./assets/lose.png"
                LoseOneLife ->
                    "./assets/continue.png"
                WaitNextStage ->
                    "./assets/continue.png"
                _ ->
                    "./assets/default.png"
        fire : Ball -> List (Svg Msg)
        fire ball=
            case ball.exist of
                True ->
                    [
                    image
                        [ x (toString (ball.xPos - 1.5))
                        , y (toString (ball.yPos - 1.2))
                        , width "3"
                        , height "3"
                        , xlinkHref "./assets/fire.png"
                        ]
                        []
                    ]
                False ->
                    [ rect [] [] ]
        fire1 = fire (Tuple.first (oneBall model "b"))
        fire2 = fire (Tuple.first (twoBall model "b"))
        fire3 = fire (Tuple.first (threeBall model "b"))
        fire4 = fire (Tuple.first (fourBall model "b"))
        fire5 = fire (Tuple.first (fiveBall model "b"))
        fire6 = fire (Tuple.first (sixBall model "b"))
        fire7 = fire (Tuple.first (sevenBall model "b"))
    in
        div [ style "width" "100%"
            , style "height" "100%"
            , style "left" "0%"
            , style "top" "0%"
            , style "background" "url(./image/bg.jpg) 0% 0% / 100% 100% no-repeat"
            , style "position" "absolute"
            , style "z-index" "-1"
            , style "overflow-y" "hidden"
            ]
            [
            img [ style "position" "absolute"
                , style "width" "40%"
                , style "height" "30%"
                , style "top" "35%"
                , style "left" "30%"
                , src propmtImage
                , alt "prompt"
                ]
                [],
            button [ onClick NextStage
                   , style "box-shadow" "0px 0px 0px 0px #f9eca0"
                   , style "background" "linear-gradient(to bottom, #f0c911 5%, #f2ab1e 100%)"
                   , style "background-color" "#f0c911"
                   , style "border-radius" "28px"
                   , style "display" "inline-block"
                   , style "cursor" "pointer"
                   , style "color" "#c92200"
                   , style "font-family" "SWGothe, Verdana"
                   , style "font-size" "1.5vw"
                   , style "text-decoration" "none"
                   , style "text-shadow" "0px 1px 16px #ded17c"
                   , style "position" "absolute"
                   , style "width" "12%"
                   , style "height" "8%"
                   , style "left" "3.5%"
                   , style "top" "62%"
                   , style "display"
                        (case model.state of
                            WaitNextStage ->
                                "block"
                            _ ->
                                "none"
                        )
                   ]
                   [ text "Next Stage"],
            button [ onClick (ChangeDisplayState DisplayInit)
                   , style "box-shadow" "inset 0px 0px 10px -20px #f29c93"
                   , style "background" "linear-gradient(to bottom, #fe1a00 5%, #ce0100 100%)"
                   , style "background-color" "#fe1a00"
                   , style "border-radius" "42px"
                   , style "display" "inline-block"
                   , style "cursor" "pointer"
                   , style "color" "#000000"
                   , style "font-family" "SWGothe, Verdana"
                   , style "font-size" "1.5vw"
                   , style "text-decoration" "none"
                   , style "text-shadow" "0px 1px 27px #b23e35"
                   , style "position" "absolute"
                   , style "width" "12%"
                   , style "height" "8%"
                   , style "left" "84%"
                   , style "top" "63%"
                   ]
                   [ text "Menu"],
            textarea [ style "width" "10%"
                     , style "height" "30%"
                     , style "color" (Tuple.second prompt)
                     , style "font-family" "Zilla Slab"
                     , style "font-weight" "500"
                     , style "font-style" "italic"
                     , style "font-size" "1.2vw"
                     , style "resize" "none"
                     , style "background" "transparent"
                     , style "border-style" "none"
                     , style "text-align" "center"
                     , style "position" "absolute"
                     , style "left" "4.5%"
                     , style "top" "28%"
                     , disabled True
                     ]
                     [ text (Tuple.first prompt) ],
            div [ style "color" "#ffffff"
                , style "font-family" "SWGothe, Arial, sans-serif"
                , style "font-size" "2vw"
                , style "left" "85%"
                , style "top" "8.5%"
                , style "position" "absolute"
                , style "display" "block"
                , style "width" "10%"
                , style "height" "5%"
                , style "text-align" "center"
                ]
                --[ text ("Score:" ++ (padLeft 5 '0' (toString model.player.score))) ],
                [ text ("Score:" ++ toString model.player.score) ],
            div [ style "color" "#ad1453"
                , style "font-family" "SWGothe, Arial, sans-serif"
                , style "font-size" "2vw"
                , style "left" "85%"
                , style "top" "25%"
                , style "position" "absolute"
                , style "display" "block"
                , style "width" "10%"
                , style "height" "5%"
                , style "text-align" "center"
                ]
                [ text ("Live(s):" ++ toString model.player.lives) ],
            div [ style "color" "#1453ad"
                , style "font-family" "SWGothe, Arial, sans-serif"
                , style "font-size" "2vw"
                , style "left" "85%"
                , style "top" "41.5%"
                , style "position" "absolute"
                , style "display" "block"
                , style "width" "10%"
                , style "height" "5%"
                , style "text-align" "center"
                ]
                [ text ("Skill:" ++ toString model.numberOfSkills) ],
                --[ text ("paddle:" ++ toString model.paddle.width) ],
            playMusic,
            sound1,
            sound2,
            Svg.svg
                [ width "auto"
                , height "auto"
                , viewBox "-25 0 125 55"
                , preserveAspectRatio "none"
                ]
                (  Block.svgBlocks model.blocks
                ++ List.map (\ball -> Ball.svgBall ball) model.balls
                ++ List.map (\fire0 -> Fire.svgFire fire0) model.fires
                ++ Paddle.svgPaddle model.paddle
                ++
                [
                rect
                    [ x "0"
                    , y "1"
                    , width "75"
                    , height "54"
                    , fillOpacity "0.2"
                    , strokeOpacity "1"
                    , stroke "#ad1453"
                    , strokeWidth "0.2"
                    , rx "1"
                    , ry "1"
                    ]
                    []
                ]
                ++
                [
                image
                    [ x "77"
                    , y "40"
                    , width "20"
                    , height "14"
                    , xlinkHref "./image/Mehmet.png"
                    ]
                    []
                ]
                ++
                [
                image
                    [ x "-22"
                    , y "40"
                    , width "20"
                    , height "14"
                    , xlinkHref "./image/CXI.png"
                    ]
                    []
                ]
                ++
                [
                image
                    [ x "-21"
                    , y "2"
                    , width "16"
                    , height "4"
                    , xlinkHref difficultyPrompt
                    ]
                    []
                ]
                ++
                [
                image
                    [ x "-21"
                    , y "6"
                    , width "16"
                    , height "4"
                    , xlinkHref stagePrompt
                    ]
                    []
                ]
                ++
                [
                image
                    [ x "-24"
                    , y "10"
                    , width "22"
                    , height "22"
                    , xlinkHref "./assets/islamFrame.png"
                    ]
                    []
                ]
                ++
                [
                image
                    [ x "80"
                    , y "1"
                    , width "15"
                    , height "9"
                    , xlinkHref "./assets/fancy.png"
                    ]
                    []
                ]
                ++
                [
                image
                    [ x "80"
                    , y "10"
                    , width "15"
                    , height "9"
                    , xlinkHref "./assets/fancy2.png"
                    ]
                    []
                ]
                ++
                [
                image
                    [ x "80"
                    , y "19"
                    , width "15"
                    , height "9"
                    , xlinkHref "./assets/fancy.png"
                    ]
                    []
                ]
                ++ fire1
                ++ fire2
                ++ fire3
                ++ fire4
                ++ fire5
                ++ fire6
                ++ fire7
                )
            ]