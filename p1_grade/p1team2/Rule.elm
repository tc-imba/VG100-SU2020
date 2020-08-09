module Rule exposing (..)
import Css exposing (..)
import Css.Transitions exposing (..)
import Css.Animations exposing (..)
import Html.Styled exposing(..)
import Html.Styled.Attributes exposing (href, style,src,css)
import VirtualDom

import Style.Button exposing(styleButton)
import Style.Title  exposing (styleTitle)

view : Html msg
view =
    div[
          style "width" "100vw", 
          style "height" "100vw", 
          style "background" "black",        
          style "overflow" "scroll",
          style "margin" "0 auto",
          style "overflow-x" "hidden",
          style "overflow-y" "hidden"]
      [div[                
      style "border-color" "silver",
      style "border-width" "15px",
      style "border-style" "outset",
      style "margin" "0 auto",
      style "width" "1000px",
      style "height" "1150px",
      style "background" "linear-gradient(135deg, rgba(206,188,155,1) 0%, rgba(85,63,50,1) 51%, rgba(42,31,25,1) 100%)"]
    [
      styleTitle[style "width" "100%"][text "Game Rules"],
      div[style "margin-left" "20%",style "margin-right" "20%"][h1[style "color" "white"][text "Winning Condition"]],
      div[style "margin-left" "20%",style "margin-right" "20%"][h2[style "color" "white"][text "1. Your score reaches 1000 points."]],
      div[style "margin-left" "20%",style "margin-right" "20%"][h2[style "color" "white"][text "2. Your opponent's ball falls off the board."]],
      div[style "margin-left" "20%",style "margin-right" "20%"][h2[style "color" "white"][text "3. Your opponent's block reaches the bottom."]],
      div[style "margin-left" "20%",style "margin-right" "20%"][h1[style "color" "white"][text "Game Setting"]],
      div[style "margin-left" "20%",style "margin-right" "20%"][h2[style "color" "white"][text "You can choose your hero at the bottom of the screen, each hero has his unique skill."]],
      div[style "margin-left" "20%",style "margin-right" "20%"][h2[style "color" "white"][text "Player1 Use A and D to control the bat and S to perfrom active skill."]],
      div[style "margin-left" "20%",style "margin-right" "20%"][h2[style "color" "white"][text "Player2 Use ← and → to control the bat and ↓ to perfrom active skill."]],
      div[style "margin-left" "20%",style "margin-right" "20%"][h2[style "color" "white"][text "When one player clear a line, the other player will get an addidtional line from the top"]],
      div[style "margin-left" "20%",style "margin-right" "20%"][h2[style "color" "white"][text "Each player can only use active skill once"]],
      div[style "margin-left" "20%",style "margin-right" "20%"][h1[style "color" "white"][text "Scoring Rule"]],
      div[style "margin-left" "20%",style "margin-right" "20%"][img[style "width" "600px",src "./images/points.jpg"][]],
      a[href "index.html"][styleButton[style "margin-left" "43%",style "width" "120%",css[Css.marginTop (px 50)]][text "Back to Menu" ]]
    ]]

    
main : VirtualDom.Node msg
main =
    toUnstyled <| view 