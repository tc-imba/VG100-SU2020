module Store exposing (..)

import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Message exposing (Msg(..))
import Model exposing (..)
import String exposing (String)


viewStore : Model.Model -> Html Msg
viewStore model =
    let
        atkCost =
            model.properties.atk * 50

        spdCost =
            model.properties.spd * 50
    in
    div
        [ style "text-align" "center"
        , style "background-color" "#3b3c4f"
        , style "position" "absolute"
        , style "width" "100%"
        , style "height" "100%"
        ]
        [ viewTitle
        , div
            [ style "background-color" "#feeacc"
            , style "font-family" "georgia"
            , style "width" "800px"
            , style "height" "450px"
            , style "margin" "auto"
            , style "border" "inset"
            , style "border-radius" "4px"
            , style "border-width" "10px"
            , style "border-color" "#ff9100"
            ]
            [ br [] []
            , p
                [ style "width" "350px"
                , style "text-align" "center"
                , style "margin-left" "225px"
                , style "font-family" "georgia"
                , style "font-size" "22px"
                ]
                [ "You now have " ++ toString model.asset ++ " coins." |> Html.text ]
            
            , div
                [ style "font-size" "20px"
                , style "margin" "auto 20px"
                , style "text-align" "center"
                ]
                [ drawScroll model True
                , button
                    ([ Html.Events.onClick Message.UGatk
                     , style "font-size"
                        "30px"
                     , style
                        "display"
                        "inline-block"
                     ]
                        ++ (if model.properties.atk < 4 then
                                bstyle "#008535"

                            else
                                bstyle "#a72525"
                           )
                    )
                    [ if model.properties.atk < 4 then
                        Debug.toString atkCost ++ "coins" |> text

                      else
                        "MAX" |> text
                    ]
                ]
            , div
                [ style "font-size" "20px"
                , style "margin" "20px 20px"
                , style "text-align" "center"
                ]
                [ text "Attack's value refers to how many HPs that the ball can harm a block at a time"
                , br [] []
                , br [] []
                , br [] []
                , text "e.g. You can eliminate a HP-4 block at ONE time with an attack-4 ball."
                , br [] []
                , text "However, you must eliminate a HP-4 block at FOUR times with an attack-1 ball"
                , br [] []
                , br [] []
                , text "Hint: THIS MAY HELP YOU WITH THE FINAL LEVEL!!!"
                ]
            ]
        , button
            ([ Html.Events.onClick ClickHome, style "height" "1.5cm" ]
                ++ bstyle "#2e72cc"
            )
            [ div
                [ style "height" "1cm"
                , style "font-family" "georgia"
                , style "color" "#FFFFFF"
                , style "font-size" "30px"
                , style "line-height" "1cm"
                ]
                [ text "Back" ]
            ]
        ]


filledBlock : Html Msg
filledBlock =
    div
        [ style "background" "#AE01AB"
        , style "border" "groove"
        , style "width" "25px"
        , style "height" "40px"
        , style "margin" "auto"
        , style "display" "inline-block"
        ]
        []


emptyBlock : Html Msg
emptyBlock =
    div
        [ style "border-color" "#AE01AB"
        , style "border" "groove"
        , style "width" "25px"
        , style "height" "40px"
        , style "margin" "auto"
        , style "display" "inline-block"
        ]
        []


bstyle : String -> List (Attribute msg)
bstyle clr =
    [ style "width" "100px"
    , style "background-color" clr
    , style "color" "white"
    , style "padding" "12px 12px"
    , style "border" "none"
    , style "border-radius" "4px"
    , style "font-size" "20px"
    , style "cursor" "pointer"
    , style "display" "block"
    , style "margin" "12px auto"
    ]


viewTitle : Html Msg
viewTitle =
    div []
        [ div
            []
            [ Html.img
                [ src "https://i.loli.net/2020/06/10/9SjnmMGse7krgXT.png"
                , style "height" "2cm"
                , style "margin-top" "50px"
                ]
                []
            , div
                [ style "font-family" "georgia"
                , style "font-size" "20px"
                , style "color" "#ffffff"
                , style "margin-bottom" "0.7cm"
                ]
                [ text "You can upgrade attributes here!" ]
            ]
        ]


drawScroll : Model.Model -> Bool -> Html Msg
drawScroll model flag =
    let
        var =
            if flag == True then
                toFloat model.properties.atk

            else
                model.properties.spd

        txt =
            div
                [ style "margin-right" "10px"
                , style "margin-bottom" "20px"
                , style "text-align" "center"
                ]
                [ (if flag == True then
                    "Attack of the ball"

                   else
                    "Speed of the ball"
                  )
                    |> Html.text
                ]
    in
    case floor var of
        1 ->
            div
                [ style "margin-left" "auto"
                , style "display" "inline-block"
                ]
                [ txt
                , filledBlock
                , emptyBlock
                , emptyBlock
                , emptyBlock
                ]

        2 ->
            div
                [ style "margin-left" "auto"
                , style "display" "inline-block"
                ]
                [ txt
                , filledBlock
                , filledBlock
                , emptyBlock
                , emptyBlock
                ]

        3 ->
            div
                [ style "margin-left" "auto"
                , style "display" "inline-block"
                ]
                [ txt
                , filledBlock
                , filledBlock
                , filledBlock
                , emptyBlock
                ]

        4 ->
            div
                [ style "margin-left" "auto"
                , style "display" "inline-block"
                ]
                [ txt
                , filledBlock
                , filledBlock
                , filledBlock
                , filledBlock
                ]

        _ ->
            div [] []
