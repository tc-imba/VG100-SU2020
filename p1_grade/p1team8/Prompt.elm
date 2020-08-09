module Prompt exposing (..)

import Boundary exposing (..)
import Html exposing (..)
import Instructions.Combination exposing (..)
import Instructions.Passage exposing (..)
import Instructions.Picture exposing (..)
import Levels.Instr exposing (..)
import Msg exposing (..)
import Svg
import Svg.Attributes as SvgAttrs


type alias Prompt =
    { win : Combination
    , lose : Combination
    , welcome : Combination
    , instruct : Pic
    , curinst : Passage
    }


initprompt : ( Float, Float ) -> Int -> Prompt
initprompt boundsize levelnum =
    let
        ( windw, windh ) =
            boundsize

        win =
            initpassage ( windw / 2, windh / 2 )
                (windw / 10.0)
                3000
                [ [ "All of a sudden, you woke up from the desk", "You look around with confusion..."], 
                  ["This is YuLiMing Student Center at 12:30pm", "You were just having a dream!" ],
                  [ "You look at the screen of your computer... ", "The VPN has just been connected. " ], 

                  ["The End ->"], 
                  ["Congratulations! ", "You successfully ﬂed from Baibai,", " and now you can use Googol!", "The reward is quite worthwhile, isn’t it?"], 
                  ["But is Googol (or BingBing) a cure for all?"], 
                  ["Living in an era of information is challenging,", "Various of search engines", "provide all kinds of information. "],
                  ["It is our duty to distinguish them.", "Because whatever engines we use. . . ", "Blind trust always brings consequences."], 
                  ["Residents in the era of information — ", "sharp your eyes and minds."]
                , [ "Gamer" ]
                ]

        lose =
            initpassage ( windw / 2, windh / 2 ) (windw / 10.0) 3000 [ [ "Sorry", "You LOSE" ], [ "Gamer" ] ]

        levelinstr =
            getlevelinstr levelnum ( windw / 2, windh / 2 ) (windw / 30)

        welc =
            initpassage ( windw / 2, windh / 2 ) (windw / 10.0) 1000 [ [ "NetFlee" ] ]

        logo =
            initpic "https://s1.ax1x.com/2020/06/11/tHYe2t.png"

        illu =
            initpic "https://s1.ax1x.com/2020/06/11/tb8anS.md.png"

        instr = 
            initpic "https://s1.ax1x.com/2020/06/17/NAw7jg.png"
    in
    { welcome = initcombination welc illu, instruct = instr, win = initcombination win logo, lose = initcombination lose logo, curinst = levelinstr }


viewprompt : GameState -> ( Float, Float ) -> Prompt -> Html Msg
viewprompt state bound model =
    let
        ( windw, windh ) =
            bound

        picsize = 
            (min windw windh)

    in
    case state of
        Welcome ->
            viewcombination bound (min windw windh) model.welcome

        Win ->
            viewcombination bound (min windw windh / 2) model.win

        Lose ->
            viewcombination bound (min windw windh / 2) model.lose

        LevelInstr ->
            Svg.svg
                [ SvgAttrs.width (String.fromFloat windw)
                , SvgAttrs.height (String.fromFloat windh)
                , SvgAttrs.viewBox ("0 0 " ++ String.fromFloat windw ++ " " ++ String.fromFloat windh)
                ]
                [ renderpromptbackgrd ( 0, 0 ) bound
                , passagechangesize (windw / 30) model.curinst
                    |> passagechangepos ( windw / 2, windh / 2 )
                    |> viewpassage
                ]

        _ ->
            Svg.svg
                [ SvgAttrs.width "0"
                , SvgAttrs.height "0"
                ]
                []


updateprompt : GameState -> Float -> Prompt -> Prompt
updateprompt state elapsed model =
    case state of
        Welcome ->
            { model | welcome = updatecombination elapsed model.welcome }

        Win ->
            { model | win = updatecombination elapsed model.win }

        Lose ->
            { model | lose = updatecombination elapsed model.lose }

        LevelInstr ->
            { model | curinst = updatepassage elapsed model.curinst }

        _ ->
            model
