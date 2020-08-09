module P1team17 exposing (main)
import Browser
import Browser.Dom exposing (getViewport,Viewport)
import Browser.Events exposing (..)
import Task
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (style)
import Json.Decode
import Json.Encode exposing (Value)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import P1team17type exposing (..)
import P1team17set exposing (..)
--Main--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
main=
  Browser.element
    {init=init
    ,subscriptions=subscriptions
    ,update=update
    ,view=view
    }
--Main--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--Init--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
init:()->(Model,Cmd Msg)
init _=
  (initmodel
  ,Task.perform MsgGetViewport getViewport
  )
--Init--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--Subscriptions--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
keymap:Bool->Int->Msg
keymap keysta code=
  case code of
    32->--space
      if keysta then
        Msgsta
      else
        Msgnul
    37->Msgl keysta
    38->Msgu keysta
    39->Msgr keysta
    40->Msgd keysta
    _->Msgnul
subscriptions:Model->Sub Msg
subscriptions model=
  Sub.batch
    [ if model.sta==0||model.sta==5 then
        Browser.Events.onAnimationFrameDelta Msgtik
      else
        Sub.none
    ,Browser.Events.onClick(Json.Decode.succeed(MsgMouseClick))
    ,Browser.Events.onMouseDown(Json.Decode.succeed(MsgMouseButton True))
    ,Browser.Events.onMouseUp(Json.Decode.succeed(MsgMouseButton False))
    ,Browser.Events.onMouseMove(Json.Decode.map2 MsgMouseMove (Json.Decode.field "pageX" Json.Decode.float) (Json.Decode.field "pageY" Json.Decode.float))
    ,Browser.Events.onKeyDown(Json.Decode.map(keymap True)keyCode)
    ,Browser.Events.onKeyUp(Json.Decode.map(keymap False)keyCode)
    ,Browser.Events.onResize Msgrsz
    ]
--Subscriptions--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--Update--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
ctrlbrd1:Model->Float->Board
ctrlbrd1 model tt=
  let
    brdposx1=
      if model.brdmovl&&(not model.brdmovr) then
        model.brd.posx-brdp.spdx*tt
      else if (not model.brdmovl)&&model.brdmovr then
        model.brd.posx+brdp.spdx*tt
      else
        model.brd.posx
    brdposy1=
      if model.brdmovu&&(not model.brdmovd) then
        model.brd.posy-brdp.spdy*tt
      else if (not model.brdmovu)&&model.brdmovd then
        model.brd.posy+brdp.spdy*tt
      else
        model.brd.posy
  in
  {posx=
    if brdposx1<brdp.deltx then
      brdposx1+boxp.wid
    else if brdposx1>(boxp.wid+brdp.deltx) then
      brdposx1-boxp.wid
    else
      brdposx1
  ,posy=
    if brdposy1<brdp.delty then
      brdposy1+boxp.hei
    else if brdposy1>(boxp.hei+brdp.delty) then
      brdposy1-boxp.hei
    else
      brdposy1
  }
chkbrd:Ball->Board->(Ball,Bool)
chkbrd bal1 brd=
  if (bal1.posx+balp.r<brd.posx)||(bal1.posx-balp.r>brd.posx+brdp.wid)||(bal1.posy<brd.posy-balp.r)||(bal1.posy>brd.posy+brdp.hei+balp.r) then
    (bal1,False)
  else
    let
      (brdmidx,brdmidy)=((brd.posx+brdp.wid/2),(brd.posy+brdp.hei/2))
      (tx1,ty1)=(bal1.posx-brdmidx,bal1.posy-brdmidy)
    in
    if((abs tx1)<(abs ty1))then
      let
        (posy1,spdy1)=
          if (bal1.posy<brdmidy) then
            (brd.posy-balp.r,-(abs(bal1.spdy)))
          else
            (brd.posy+brdp.hei+balp.r,(abs(bal1.spdy)))
      in
      ({bal1|posy=posy1,spdy=spdy1},True)
    else
      let
        (posx1,spdx1)=
          if (bal1.posx<brdmidx) then
            (brd.posx-balp.r,-(abs(bal1.spdx)))
          else
            (brd.posx+brdp.wid+balp.r,(abs(bal1.spdx)))
      in
      ({bal1|posx=posx1,spdx=spdx1},True)
chkblk:Ball->List Block->BallState->Float->{bal:Ball,blk:List Block,balsta:BallState,balcont:Bool}
chkblk bal1 blk balsta0 tt=
  let
    lst1=
      if balsta0.buffcont>0 then
        List.map (chkthisblkpenetrate bal1 tt) blk
      else
        List.map (chkthisblk bal1 tt) blk
    lst2=List.filter (\x->x.balcont>0) lst1
    lst3=List.filter (\x->x.balcont==1) lst1
    balsta1=List.foldl (\x it->addbalsta x.deltbalsta it) balsta0 lst2
    balsta2={balsta1|health=(Basics.min balsta1.health (healthlim balsta1))}
    (bal2,balcont1)=
      case (List.head lst3) of
        Just res1->(res1.bal,True)
        Nothing->(bal1,False)
  in
  {bal=bal2
  ,blk=
    if (List.isEmpty lst2) then
      blk
    else
      List.map .thisblk lst1
      |>List.filter (\x->x.sta>0)
  ,balsta=balsta2
  ,balcont=balcont1
  }
update:Msg->Model->(Model,Cmd Msg)
update msg model=
  case msg of
    Msgtik tt->
      let
        brd1=
          if (model.mousebutton)&&(model.mousex<=boxp.wid)&&(model.mousey<=boxp.hei) then
            {posx=model.mousex-brdp.wid/2,posy=model.mousey-brdp.hei/2}
          else
            ctrlbrd1 model tt
        bal0=model.bal
        bal1={bal0|posx=model.bal.posx+model.bal.spdx*tt,posy=model.bal.posy+model.bal.spdy*tt}
      in
      if bal1.posx<0 then
        (nxtmodel0 {model|brd=brd1,bal=bal1} 3
        ,Cmd.none
        )
      else if bal1.posx>boxp.wid then
        (nxtmodel0 {model|brd=brd1,bal=bal1} 1
        ,Cmd.none
        )
      else if bal1.posy<0 then
        (nxtmodel0 {model|brd=brd1,bal=bal1} 4
        ,Cmd.none
        )
      else if bal1.posy>boxp.hei then
        (nxtmodel0 {model|brd=brd1,bal=bal1} 2
        ,Cmd.none
        )
      else
        let
          (bal2,cont1)=chkbrd bal1 brd1
          res1=
            if model.sta==5 then
              truendchkblk bal1 model.blk model.balsta tt
            else
              chkblk bal1 model.blk model.balsta tt
          bal3=res1.bal
          blk1=res1.blk
          balsta1=res1.balsta
          cont2=res1.balcont
          balsta2=
            if balsta1.buffcont>0 then
              {balsta1|buffcont=balsta1.buffcont-tt}
            else
              balsta1
        in
        ( {model
            |brd=brd1
            ,bal=
              if cont2 then
                bal3
              else
                bal2
            ,blk=blk1
            ,balsta=balsta2
            ,sta=
              if (model.balsta.health<=0) then
                3
              else if (model.curset.index==0)&&(List.isEmpty(List.filter (\t1->t1.typ==2) blk1)) then
                4
              else
                model.sta
          }
        ,Cmd.none
        )
    Msgsta->
      case model.sta of
        0->({model|sta=1},Cmd.none)
        1->({model|sta=0},Cmd.none)
        2->({model|sta=0},Cmd.none)
        3->
          (initmodel
          ,Task.perform MsgGetViewport getViewport
          )
        4->
          (truendmodel
          ,Task.perform MsgGetViewport getViewport
          )
        5->
          (initmodel
          ,Task.perform MsgGetViewport getViewport
          )
        _->(model,Cmd.none)
    Msgl keysta->
      ({model|brdmovl=keysta}
      ,Cmd.none
      )
    Msgr keysta->
      ({model|brdmovr=keysta}
      ,Cmd.none
      )
    Msgu keysta->
      ({model|brdmovu=keysta}
      ,Cmd.none
      )
    Msgd keysta->
      ({model|brdmovd=keysta}
      ,Cmd.none
      )
    Msgrsz wid1 hei1->
      let
        windowwid1=(toFloat wid1)
        windowhei1=(toFloat hei1)
      in
      ( {model
          |windowwid=windowwid1
          ,windowhei=windowhei1
          ,sca=Basics.min (windowwid1/viewp.wid) (windowhei1/viewp.hei)
        }
      ,Cmd.none
      )
    MsgGetViewport viewport->
      let
        windowwid1=viewport.viewport.width
        windowhei1=viewport.viewport.height
      in
      ( {model
          |windowwid=windowwid1
          ,windowhei=windowhei1
          ,sca=Basics.min (windowwid1/viewp.wid) (windowhei1/viewp.hei)
        }
      ,Cmd.none
      )
    MsgMouseClick->--double_click
      (model
      ,Cmd.none
      )
    MsgMouseButton keysta->
      ({model|mousebutton=keysta}
      ,Cmd.none
      )
    MsgMouseMove mousepagex mousepagey->
      ({model|mousex=mousepagex/model.sca,mousey=mousepagey/model.sca}
      ,Cmd.none
      )
    _->
      (model
      ,Cmd.none
      )
--Update--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--View--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

--render//
renderCharHealthBar : Model -> Html Msg
renderCharHealthBar model =
  svg
    [viewBox("0 0 170 20")
    ,width(String.fromFloat(model.sca*170.0))
    ,height(String.fromFloat(model.sca*20.0))
--    ,Html.Attributes.style "left" (String.fromInt(round(model.sca*20.0))++"px")
    ,Html.Attributes.style "padding" (String.fromInt(round(model.sca*3.0))++"px")
    ]
    [ rect
        [Svg.Attributes.x("0")
        ,Svg.Attributes.y("0")
        ,Svg.Attributes.width("170")
        ,Svg.Attributes.height("20")
        ,Svg.Attributes.fill("#7f7f7f")
        ,Svg.Attributes.strokeWidth("0")
        ]
        []
    , rect
        [Svg.Attributes.x("0")
        ,Svg.Attributes.y("0")
        ,Svg.Attributes.width(String.fromFloat(170.0*model.balsta.health/(healthlim model.balsta)))
        ,Svg.Attributes.height("20")
        ,Svg.Attributes.fill("#8c0015")
        ,Svg.Attributes.strokeWidth("0")
        ]
        []
    ]
{--
  button
    [ Html.Attributes.style "background" "#8c0015"
    , Html.Attributes.style "border" "0"
--    , Html.Attributes.style "top" "76px"
--    , Html.Attributes.style "color" "#000000"
    , Html.Attributes.style "display" "block"
    , Html.Attributes.style "font-weight" "0"
    , Html.Attributes.style "height" "15px"
--    , Html.Attributes.style "left" "65px"
--    , Html.Attributes.style "line-height" "60px"
    , Html.Attributes.style "outline" "none"
    , Html.Attributes.style "padding" "0"
--    , Html.Attributes.style "position" "absolute"
--    , Html.Attributes.style "font-size" "14px"
    , Html.Attributes.style "width" (String.fromInt (round (model.balsta.health / 5))++"px")
    ]
    []
--}

renderGameTitle : Model -> Html Msg
renderGameTitle model =
    div []
        [ h1
        [ Html.Attributes.style "color" "#34495f"
        , Html.Attributes.style "font-size" (String.fromInt(round(model.sca*17.0))++"px")
        ]
        [ Html.text "The UNSTOPPABLE" ]
        ]

renderCharHealth : BallState -> Html Msg
renderCharHealth balsta =
    div
        [ Html.Attributes.style "color" "#34495f"
        , Html.Attributes.style "font-size" "20px"
        ]
        [ Html.text (String.fromInt(round balsta.health)++"/"++String.fromInt(round(healthlim balsta)))]


renderCharBuff : Float -> Html Msg
renderCharBuff buff =
    if buff > 0 then
    div
        [ Html.Attributes.style "color" "#405bd0"
        , Html.Attributes.style "font-size" "18px"
        ]
        [ Html.text ("Penetrate:"++(String.fromFloat(buff/1000.0))++"s") ]
    else
    div
        [ Html.Attributes.style "color" "#34495f"
        , Html.Attributes.style "font-size" "18px"
        ]
        [ Html.text ("Penetrate:None") ]


renderPanel : Model -> Html Msg
renderPanel model  =
    let
      statxt=
          case model.sta of
              0->"pause"
              1->"resume"
              2->"start"
              3->"restart"
              4->"continue"
              5->"return"
              _->""
      poshint=
          if model.sta==5 then
              div
                  [Html.Attributes.style "color" "#dfdfdf"
                  , Html.Attributes.style "font-size" "18px"
                  ]
                  [ Html.text "The end" ]
          else if model.curset.index>24 then
              div
                  [Html.Attributes.style "color" "#34495f"
                  , Html.Attributes.style "font-size" "18px"
                  ]
                  [ Html.text("Dungeon,section"++(String.fromInt(model.curset.index))) ]
          else if model.curset.index>0 then
              div
                  [Html.Attributes.style "color" "#34495f"
                  , Html.Attributes.style "font-size" "18px"
                  ]
                  [ Html.text("Island,section"++(String.fromInt(model.curset.index))) ]
          else
              div
                  [Html.Attributes.style "color" "#8c0015"
                  , Html.Attributes.style "font-size" "18px"
                  ]
                  [ Html.text "The Void" ]
    in
    div
        [ Html.Attributes.style "top" "0"
        , Html.Attributes.style "bottom" "0px"
        , Html.Attributes.style "left" ((String.fromFloat(boxp.wid*model.sca))++"px")
        , Html.Attributes.style "right" "0"
--        , Html.Attributes.style "color" "#34495f"
--        , Html.Attributes.style "font-size" "7px"
--        , Html.Attributes.style "padding" "0 30px"
        , Html.Attributes.style "position" "absolute"
        ]
        [ renderGameTitle model
        , renderCharHealthBar model
        , renderCharHealth model.balsta
--        , renderCharVal model.balsta.val
        , renderCharBuff model.balsta.buffcont
        , poshint
        , button
              [ Html.Attributes.style "width" (String.fromFloat(model.sca*47.0)++"px")
              , Html.Attributes.style "height" (String.fromFloat(model.sca*25.0)++"px")
              , Html.Attributes.style "padding" (String.fromInt(round(model.sca*3.0))++"px")
--              , Html.Attributes.style "left" (String.fromInt(round(model.sca*20.0))++"px")
--              , Html.Attributes.style "line-height" "2px"
              , Html.Attributes.style "display" "block"
              , Html.Attributes.style "background" "#c0c0c0"
              , Html.Attributes.style "border" "0"
              , Html.Attributes.style "color" "#34495f"
              , Html.Attributes.style "font-weight" "0"
              , Html.Attributes.style "outline" "none"
              , Html.Attributes.style "font-size" (String.fromInt(round(model.sca*12.0))++"px")
              , Html.Events.onClick(Msgsta)
              ]
              [Html.text statxt]
        , div
              [ Html.Attributes.style "color" "#7f7f7f"
              , Html.Attributes.style "font-size" "15px"
              ]
              [ Html.text ("Use mouse or arrow keys to move the board.") ]
        , div
              [ Html.Attributes.style "color" "#7f7f7f"
              , Html.Attributes.style "font-size" "15px"
              ]
              [ Html.text ("Press space to "++statxt++".") ]
        ]


--render//

thisblkfig:Block->Svg Msg
thisblkfig thisblk=
  use
    [Svg.Attributes.xlinkHref("#reuse"++String.fromInt(thisblk.typ))
    ,Svg.Attributes.x(String.fromFloat(thisblk.posx))
    ,Svg.Attributes.y(String.fromFloat(thisblk.posy))
    ,Svg.Attributes.width("")
    ,Svg.Attributes.height("")
    ]
    []
view:Model->Html Msg
view model=
  let
    bkgfig=
      use
        [Svg.Attributes.xlinkHref("#reusebkg")
        ,Svg.Attributes.x("0")
        ,Svg.Attributes.y("0")
        ,Svg.Attributes.width("")
        ,Svg.Attributes.height("")
        ]
        []
    brdfig=
      image
        [Svg.Attributes.xlinkHref(brdurl model)
        ,Svg.Attributes.x(String.fromFloat(model.brd.posx))
        ,Svg.Attributes.y(String.fromFloat(model.brd.posy))
        ,Svg.Attributes.width(String.fromFloat(brdp.wid))
        ,Svg.Attributes.height(String.fromFloat(brdp.hei))
        ]
        []
    balfig=
      image
        [ Svg.Attributes.xlinkHref(
            if model.bal.spdy<0 then
              "res/balu.gif"
            else
              "res/bald.gif"
          )
        ,Svg.Attributes.x(String.fromFloat(model.bal.posx-16.0))
        ,Svg.Attributes.y(String.fromFloat(model.bal.posy-16.0))
        ,Svg.Attributes.width("32")
        ,Svg.Attributes.height("32")
        ]
        []
    blkfig=List.map thisblkfig model.blk
    buffefffig=
      image
        [Svg.Attributes.xlinkHref("res/buffeff.png")
        ,Svg.Attributes.x(String.fromFloat(model.bal.posx-balp.r-8))
        ,Svg.Attributes.y(String.fromFloat(model.bal.posy-balp.r))
        ,Svg.Attributes.width(String.fromFloat(balp.r*2))
        ,Svg.Attributes.height(String.fromFloat(balp.r*2))
        ,Svg.Attributes.transform("rotate("++String.fromFloat((atan2 model.bal.spdy model.bal.spdx)/pi*180)++" "++String.fromFloat(model.bal.posx)++" "++String.fromFloat(model.bal.posy)++")")
        ]
        []
    svglst1=
      if model.balsta.buffcont>0 then
        [brdfig,buffefffig,balfig]
      else
        [brdfig,balfig]
    gamehtml=
      svglst1
      |>List.append blkfig
      |>List.append [model.reuse,bkgfig]
      |>svg[(viewBox("0 0 "++(String.fromFloat boxp.wid)++" "++(String.fromFloat boxp.hei))),(width(String.fromFloat(model.sca*boxp.wid))),(height(String.fromFloat(model.sca*boxp.hei)))]
  in
  case model.sta of
    2->
      let
        svglst2=
            [image
              [Svg.Attributes.xlinkHref("res/title.png")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width(String.fromFloat(boxp.wid))
              ,Svg.Attributes.height(String.fromFloat(boxp.hei))
              ]
              []
            ]
            |>List.append blkfig
            |>List.append [model.reuse,bkgfig]
      in
      div
        []
        [ svg
            [(viewBox("0 0 "++(String.fromFloat boxp.wid)++" "++(String.fromFloat boxp.hei))),(width(String.fromFloat(model.sca*boxp.wid))),(height(String.fromFloat(model.sca*boxp.hei)))]
            svglst2
        ,renderPanel model
        ]
    3->
      div
        []
        [ div
            [Html.Attributes.style "top" "0"
            ,Html.Attributes.style "bottom" "0"
            ,Html.Attributes.style "left" "0"
            ,Html.Attributes.style "right" "0"
            ,Html.Attributes.style "position" "absolute"
            ,Html.Attributes.style "background" "#7f7f7f"
            ]
            [ div
                [Html.Attributes.style "color" "#8c0015"
                ,Html.Attributes.style "font-size" (String.fromInt(round(model.sca*boxp.wid/8))++"px")
                ]
                [Html.text("YOU DIED")]
            ]
        ,renderPanel model
        ]
    4->
      div
        []
        [ div
            [Html.Attributes.style "top" "0"
            ,Html.Attributes.style "bottom" "0"
            ,Html.Attributes.style "left" "0"
            ,Html.Attributes.style "right" "0"
            ,Html.Attributes.style "position" "absolute"
            ,Html.Attributes.style "background" "#7f7f7f"
            ]
            [ div
                [Html.Attributes.style "color" "#dfdfdf"
                ,Html.Attributes.style "font-size" (String.fromInt(round(model.sca*boxp.wid/8))++"px")
                ]
                [Html.text("THE END")]
            , div
                [Html.Attributes.style "color" "#dfdfdf"
                ,Html.Attributes.style "font-size" "18px"
                ]
                [Html.text("The unnamable mess of flesh in the void ")]
            , div
                [Html.Attributes.style "color" "#dfdfdf"
                ,Html.Attributes.style "font-size" "18px"
                ]
                [Html.text("has been annihilated.")]
            , div
                [Html.Attributes.style "color" "#dfdfdf"
                ,Html.Attributes.style "font-size" "18px"
                ]
                [Html.text("Your daughter has returned.")]
            , div
                [Html.Attributes.style "color" "#dfdfdf"
                ,Html.Attributes.style "font-size" "18px"
                ]
                [Html.text("Good night, warrior.")]
            , div
                [Html.Attributes.style "color" "#dfdfdf"
                ,Html.Attributes.style "font-size" "18px"
                ]
                [Html.text("Don't forget to tell your daughter ")]
            , div
                [Html.Attributes.style "color" "#dfdfdf"
                ,Html.Attributes.style "font-size" "18px"
                ]
                [Html.text("a bedtime story.")]
            ]
        ,renderPanel model
        ]
    _->
      div
        []
        [gamehtml
        ,renderPanel model
        ,audio [Html.Attributes.src "res/bgm01.mp3", Html.Attributes.autoplay True, Html.Attributes.loop True][]
        ]
--View--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
