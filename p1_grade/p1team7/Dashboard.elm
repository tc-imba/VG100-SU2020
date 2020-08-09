module Dashboard exposing (..)
import Svg exposing (image, rect,svg,tspan,text_)
import Svg.Attributes exposing (x,y,width,height,fill,fontSize,xlinkHref,opacity,visibility,fillOpacity)
import Svg.Events exposing (onClick)
import Html exposing (..)
import Html.Attributes exposing (src, style,href)
import Model exposing (..)
import Color exposing (..)
import Message exposing (..)
import Outlooks exposing (..)
import Ionicon -- some svg icons, may be helpful later
import List.Extra exposing (interweave,count,getAt)


renderPanel: Point -> Float -> Float -> Html msg
renderPanel point wid hei =
   rect [x ((String.fromFloat point.x)++"%"), y ((String.fromFloat point.y)++"%"), width ((String.fromFloat wid)++"%"), height ((String.fromFloat hei)++"%") ,fill "#393938f3",opacity "0.5"][]



renderStatus : Model -> List(Html Msg)
renderStatus model =
     let
            life = svg[x "40%", y"1%"][Ionicon.heart 7 red]
            star = svg[x "40%", y"4%"][Ionicon.star 7 yellow]
            leaf = svg[x "48%", y"1%"][Ionicon.leaf 7 green]
            clock = svg[x "48%", y"4%"][Ionicon.clock 7 deep_grey]
            out_minute = String.fromInt (model.minute)
            out_second = String.fromInt (model.second)
     in
        [
             life
            ,text_[x "42%", y"2.5%",fill "white",fontSize "4"][text  ("Life: "++ String.fromInt model.life ++ "/" ++ String.fromInt model.max_life)]
            ,star
            ,text_[x "42%", y"5.5%",fill "white",fontSize "4"][text ("Exp: "++String.fromInt model.exp)]
            ,leaf
            ,if model.leaf /= 1 then text_[x "50%", y"2.5%",fill "white",fontSize "4"][text (String.fromInt model.leaf ++ " Clovers collect")] else text_[x "50%", y"2.5%",fill "white",fontSize "4"][text (String.fromInt model.leaf ++ " Clover collect")]
            ,clock
            ,text_[x "50%", y"5.5%",fill "white",fontSize "4"][ text ("Time Played" ++ ":" ++ (out_minute) ++ ":" ++ out_second)]
            --,text_[x "50%", y"6.5%",fill "black",fontSize "4"][ text ("Next Attack: " ++ next_pos)]
        ]

hype = rect[x "62%", y"1.2%",width"2%",height"2%",fillOpacity"0",onClick GoHome][]
renderSettings : List(Html Msg)
renderSettings =
    let

         refresh = svg[x "59%", y"2.2%"][Ionicon.refresh 9 grey]
         home = svg[x "62%", y"1%"][Ionicon.home 9 grey]
         help = svg[x "62%", y"3.8%"][Ionicon.help 9 grey]
         hype1 = rect[x "62%", y"1.2%",width"2%",height"2%",fillOpacity"0",onClick GoHome][]  -- 对于home和help的超链接
         hype2 = rect[x "62%", y"4%",width"2%",height"2%",fillOpacity"0",onClick GoHelp][] --* hidden和fill none 都不行!
         hype3 = rect[x "59%", y"2.2%",width"2%",height"2%",fillOpacity"0",onClick Start][] --* hidden和fill none 都不行!
    in
            [
                refresh
                ,home
                ,help
                ,hype1
                ,hype2
                ,hype3
            ]


renderSkills : Model ->List(Html Msg)
renderSkills model =
    let
        stx = 65
        wid = 2.5
        t1 = image[x ((String.fromFloat (stx+wid*0))++"%"), y"1%",width "2%", height"2%",xlinkHref trait8][]  -- 生命值+1生命上限+1(一次性)
        t2 = image[x ((String.fromFloat (stx+wid*1))++"%"), y"1%",width "2%", height"2%",xlinkHref trait11][] -- 技能花费-10(一次性)
        t3 = image[x ((String.fromFloat (stx+wid*2))++"%"), y"1%",width "2%", height"2%",xlinkHref trait13][] -- 踏板加速(持续性)
        t4 = image[x ((String.fromFloat (stx+wid*3))++"%"), y"1%",width "2%", height"2%",xlinkHref trait2][] --  禁止一个发射口(持续性)
        t5 = image[x ((String.fromFloat (stx+wid*4))++"%"), y"1%",width "2%", height"2%",xlinkHref trait10][] -- 经验值获得增加(持续性)
        t6 = image[x ((String.fromFloat (stx+wid*5))++"%"), y"1%",width "2%", height"2%",xlinkHref trait6][] -- 蓝球减速(持续性)
        t7 = image[x ((String.fromFloat (stx+wid*6))++"%"), y"1%",width "2%", height"2%",xlinkHref trait1][] -- 击中+2血(持续性)
        t8 = image[x ((String.fromFloat (stx+wid*7))++"%"), y"1%",width "2%", height"2%",xlinkHref trait12][] --壳转速下降(持续性)
        t9 = image[x ((String.fromFloat (stx+wid*8))++"%"), y"1%",width "2%", height"2%",xlinkHref trait4][] -- 一击致命(持续性)
        t10 = image[x ((String.fromFloat (stx+wid*9))++"%"), y"1%",width "2%", height"2%",xlinkHref trait5][] -- 金色踏板可以防御(持续性)
    in
         [
            t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,
            (skillCost model 1),(skillCost model 2),(skillCost model 3),(skillCost model 4),(skillCost model 5),(skillCost model 6),(skillCost model 7),(skillCost model 8),(skillCost model 9),(skillCost model 10),
            (skillStatus model 1),(skillStatus model 2),(skillStatus model 3),(skillStatus model 4),(skillStatus model 5),(skillStatus model 6),(skillStatus model 7),(skillStatus model 8),(skillStatus model 9),(skillStatus model 10)
         ]

fromJust: Maybe Int -> Int
fromJust x = case x of
    Just y -> y
    Nothing -> 0

skillCost: Model->Float-> Html Msg
skillCost model num =
    let
        req_exp = getAt (floor(num) - 1) model.skills_cost
        xx = 65+ (num - 1) * 2.5
        xxx = (String.fromFloat xx) ++ "%"
    in
        text_[x xxx, y"4%",fill "white",fontSize "3"][text ("C:"++(String.fromInt (fromJust req_exp)))]



skillStatus: Model->Float-> Html Msg
skillStatus model num =
    let
       -- sta = if (getAt (floor(num) - 1) model.skills_ok == Just True) then "Yes" else "No"
        sta = if (getAt (floor(num) - 1) model.skills_ok == Just True) then Ionicon.checkmarkRound 4 green else Ionicon.closeRound 4 red
        xx = 65+ (num - 1) * 2.5
        xxx = (String.fromFloat xx) ++ "%"
    in
       -- text_[x xxx, y"5.5%",fill "black",fontSize "3"][text sta] --todo 改成图标
       svg[x xxx, y"5.2%"][sta]


