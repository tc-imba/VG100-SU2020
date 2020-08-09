module Help exposing (..)
import Html exposing (..)
import Html.Attributes exposing (controls, src, style, title)
import Html.Events exposing (onClick)
import Browser
import Json.Decode as Json
import Markdown
import Ionicon exposing (home)
import Color exposing (grey)
import Message exposing (Msg(..))
import Object exposing (renderBooklet, renderInterface)
import Outlooks exposing (..)
import Model exposing (..)
import Svg exposing (image, svg)
import Svg.Attributes exposing (viewBox)


{-main =
      Browser.sandbox { init = init, update = update, view = view }-}


view: Model-> List (Html Msg)
view model =
    [
      renderNiceOutlook model,
      div[style "backgroundColor" "#1d1d1d"]
      [renderMusic model,
      renderButton1,
      renderButton2,
      renderButton3,
      renderButton4,
      renderDifficulty1,
      renderDifficulty2,
      renderDifficulty3,
      renderHome,
      renderDifficulty model,
      renderButtonLeft,
      renderButtonRight]
    ]


renderNiceOutlook : Model -> Html Msg
renderNiceOutlook model =
    div [ style "backgroundColor" "#1d1d1d"]
                        [svg
                        [ viewBox "0 0 400 200" ][
                        renderBooklet (Point 0 0) 80 100  model.showingpage
                        ]]


renderButtonLeft: Html Msg
renderButtonLeft =
    button
        [ style "background" "#02020299"
        , style "color" "#8acce7"
        , style "cursor" "pointer"
        , style "font-family" "Chalkduster"
        , style "font-size" "40px"
        , style "font-weight" "300"
        , style "height" "90px"
        , style "width" "350px"
        , style "left" "1000px"
        , style "bottom" "600px"
        , style "position" "absolute"
        , style "border" "0"
    , onClick (Alterpage Previousone)
    ]
    [ text "Previous Page"]

renderButtonRight: Html Msg
renderButtonRight =
    button
        [ style "background" "#02020299"
        , style "color" "#8acce7"
        , style "cursor" "pointer"
        , style "font-family" "Chalkduster"
        , style "font-size" "40px"
        , style "font-weight" "300"
        , style "height" "90px"
        , style "width" "350px"
        , style "left" "1000px"
        , style "bottom" "200px"
        , style "position" "absolute"
        , style "border" "0"
        , onClick (Alterpage Nextone)
        ]
        [ text "Next Page" ]




previousOne : Model -> List String -> Maybe String
previousOne model bookletL =
    if Maybe.withDefault "a"(List.head (List.drop 1 bookletL)) == model.showingpage then List.head bookletL
    else previousOne model (List.drop 1 bookletL)

nextOne : Model -> List String -> Maybe String
nextOne model bookletL =
    if Maybe.withDefault "a"(List.head (List.drop 1 (List.reverse bookletL))) == model.showingpage then List.head (List.reverse bookletL)
    else nextOne model (List.reverse(List.drop 1 (List.reverse bookletL)))

bookletList : List String
bookletList = [page1, page2, page3, page4, page5, page6, page7, page8, page9, page10, page11]





renderButton1 : Html Msg
renderButton1 =
        button
        [  style "background" "#02020299"
        , style "color" "#28b8a9"
        , style "display" "block"
        , style "height" "60px"
        , style "left" "430px"
        , style "top" "630px"
        , style "width" "200px"
        , style "border" "0"
        , onClick (ChangeMusic TheOasis)
        ]
        [ text "Try \"The Oasis\"! (From movie \"Ready Player One\")" ]
renderButton2 : Html Msg
renderButton2 =
        button
        [   style "background" "#0202024d"
          , style "color" "#28b8a9"
          , style "display" "block"
          , style "height" "60px"
          , style "left" "630px"
          , style "top" "630px"
          , style "width" "200px"
          , style "border" "0"
        , onClick (ChangeMusic ReturnOfAncients)
        ]
        [ text "Try \"Return of Ancients\"! (From game \"WarcraftIII\")" ]

renderButton3 : Html Msg
renderButton3 =
        button
        [   style "background" "#02020299"
          , style "color" "#28b8a9"
          , style "display" "block"
          , style "height" "60px"
          , style "left" "830px"
          , style "top" "630px"
          , style "width" "200px"
          , style "border" "0"
        , onClick (ChangeMusic InSearchOfLife)
        ]
        [ text "Try \"In Search of Life\"! (From game \"Stellaris\")" ]

renderButton4 : Html Msg
renderButton4 =
        button
        [   style "background" "#0202024d"
          , style "color" "#28b8a9"
          , style "display" "block"
          , style "height" "60px"
          , style "left" "1030px"
          , style "top" "630px"
          , style "width" "200px"
          , style "border" "0"
          , onClick (ChangeMusic TheChordOfSpring)
          --, onClick GoHome
        ]
        [ text "Try \"The Chord of Spring\"! (From game \"Arknights\")" ]

renderDifficulty : Model -> Html Msg
renderDifficulty model =
    let
        content =
            case model.difficulty of
                Normal -> "Normal"
                Hard -> "Hard"
                Nightmare -> "Nightmare"
    in
    div[
        style "color" "#ffffff"
              , style "display" "block"
                  , style "height" "600x"
                  , style "left" "500px"
                  , style "top" "730px"
                  , style "width" "200px"
                  , style "border" "0"
      ]
      [text ("Current Difficulty: " ++content)]

renderDifficulty1 : Html Msg
renderDifficulty1 =
        button
        [   style "background" "#524f4f99"
          , style "color" "#2fa11a"
          , style "display" "block"
          , style "height" "30px"
          , style "left" "500px"
          , style "top" "730px"
          , style "width" "200px"
          , style "border" "0"
          , onClick (ChangeDifficulty Normal)
          , title "A wise choice."
        ]
        [ text "Normal" ]
renderDifficulty2 : Html Msg
renderDifficulty2 =
        button
        [   style "background" "#adadad99"
          , style "color" "#e0dc48"
          , style "display" "block"
          , style "height" "30px"
          , style "left" "700px"
          , style "top" "730px"
          , style "width" "200px"
          , style "border" "0"
          , onClick (ChangeDifficulty Hard)
          , title "Emm."
        ]
        [ text "Hard" ]

renderDifficulty3 : Html Msg
renderDifficulty3 =
        button
        [   style "background" "#e0d4d499"
          , style "color" "#841707"
          , style "display" "block"
          , style "height" "30px"
          , style "left" "900px"
          , style "top" "730px"
          , style "width" "200px"
          , style "border" "0"
          , onClick (ChangeDifficulty Nightmare)
          , title "Do not try it."
        ]
        [ text "Nightmare" ]



renderMusic : Model -> Html Msg
renderMusic model =
    let
        music =
            case model.music of
                Null -> ""
                ReturnOfAncients -> "assets/musics/Return of Ancients.mp3"
                TheOasis -> "assets/musics/The Oasis.mp3"
                TheChordOfSpring -> "assets/musics/The Chord of Spring.mp3"
                InSearchOfLife ->"assets/musics/In Search of Life.mp3"
    in
        div[
          style "top" "430px"
        , style "left" "530px"]
        [audio
            [ src music
            , controls True
            ] []
         ]

renderHome : Html Msg
renderHome =
    div
    [
     style "top" "30px"
   , style "left" "30px"
   , title "Is everything OK?"
   , style "position" "absolute"
   , onClick GoHome
   ]
    [div[][home 50 grey]]
