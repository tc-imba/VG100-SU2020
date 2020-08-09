module Welcome exposing (..)

import Html
import Json.Encode
import Browser
import Browser.Navigation
import Css.Global exposing (tbody)
import Css exposing (..)
import Css.Transitions exposing (background3,border3,bottom3,boxShadow3,easeInOut, transition,cubicBezier,top3,transition)
import Css.Animations exposing (backgroundColor)
import Html.Styled exposing(..)
import Html.Styled.Attributes exposing (css, href, src,id,autoplay,loop,style)
import Style.Button exposing (styleButton)



view : Model -> Html Msg
view model =
  div [ css[
      
      Css.width (vw 100),
      Css.height (vw 100),
      Css.backgroundColor (rgb 0 0 0)]
  ][div[                
      style "border-color" "silver",
      style "border-width" "15px",
      style "border-style" "outset",
      style "margin" "0 auto",
      style "width" "1000px",
      style "height" "630px",
      style "background" "black"]
    [
      div[][img [style "margin-left" "25%",style "width" "50%",style "height" "width",src "./images/outofclass.png"][]],
      div[style "margin-left" "42%",style "margin-top" "-10%"][a[href "hello.html", style "display" "block", style "width"  "150px"][ styleButton [style "width" "100%"][text "start game"]  ]],
      div[style "margin-left" "42%"][a[href "background.html",style "display" "block", style "width"  "150px"][styleButton [style "width" "100%"][text "background"]]],
      div[style "margin-left" "42%"][a[href "rule.html", style "display" "block", style "width"  "150px"][styleButton [style "width" "100%"][text "Game Rule"]]],
      div[style "margin-left" "42%"][a[href "about.html", style "display" "block", style "width"  "150px"][styleButton [style "width" "100%"][text "About Us"]]]
    ]
  ]
  

main : Program () Model Msg
main =
    Browser.sandbox
        { view = view >> toUnstyled
        , update = update
        , init = initialModel
        }


update : Msg -> Model -> Model
update msg model =
    model


type Msg
    = None


type alias Model =
    {}


initialModel : Model
initialModel =
    {}

