module P1team17set exposing (..)
import Array exposing (Array,fromList,get)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import P1team17map exposing (..)
import P1team17type exposing (..)
initbalsta={zerobalsta|health=125.0,val=150.0}
healthlim:BallState->Float
healthlim balsta=balsta.val
brdurl:Model->String
brdurl model=
  if model.balsta.val<550 then
    "res/brd0.png"
  else if model.balsta.val<1000 then
    "res/brd1.png"
  else
    "res/brd2.png"
initmodel:Model
initmodel=
  let
    initsetindex=11
    set1=Maybe.withDefault set0 (List.head(List.filter (\x->x.index==initsetindex) setlst))
  in
  {brd={posx=192.0,posy=192.0}
  ,bal={posx=90.0,posy=250.0,spdx=0.05,spdy=0.05}
  ,blk=set1.initblk
  ,balsta=initbalsta
  ,curset=set1
  ,reuse=Maybe.withDefault scene0 (Array.get set1.scenetyp scenearr)
  ,sta=2
  ,brdmovl=False
  ,brdmovr=False
  ,brdmovu=False
  ,brdmovd=False
  ,windowwid=boxp.wid
  ,windowhei=boxp.hei
  ,sca=1.0
  ,mousex=0.0
  ,mousey=0.0
  ,mousebutton=False
  }
truendmodel:Model
{--scenetyp=4
--no_buff_allowed
29
0000000000000
0000000000000
0000.....0000
000....5..000
002.xx.....00
00..xx....200
005...8....00
00.......5.00
00..5......00
000....2..000
0000.....0000
0000000000000
0000000000000
--}
truendmodel=
  let
    truendsetindex=29
    set1=Maybe.withDefault set0 (List.head(List.filter (\x->x.index==truendsetindex) setlst))
  in
  {brd={posx=192.0,posy=224.0}
  ,bal={posx=155.0,posy=155.0,spdx=0.03,spdy=0.03}
  ,blk=set1.initblk
  ,balsta=initbalsta
  ,curset=set1
  ,reuse=Maybe.withDefault scene0 (Array.get set1.scenetyp scenearr)
  ,sta=5
  ,brdmovl=False
  ,brdmovr=False
  ,brdmovu=False
  ,brdmovd=False
  ,windowwid=boxp.wid
  ,windowhei=boxp.hei
  ,sca=1.0
  ,mousex=0.0
  ,mousey=0.0
  ,mousebutton=False
  }
{--
  4  
  |  
3-+-1
  |  
  2  
--}
nxtmodel0:Model->Int->Model
nxtmodel0 model nxtsetdir=
  let
    bal0=model.bal
    (nxtsetindex,bal1)=
      case nxtsetdir of
        1->
          (model.curset.nxtr
          ,{bal0|posx=bal0.posx-boxp.wid}
          )
        2->
          (model.curset.nxtd
          ,{bal0|posy=bal0.posy-boxp.hei}
          )
        3->
          (model.curset.nxtl
          ,{bal0|posx=bal0.posx+boxp.wid}
          )
        4->
          (model.curset.nxtu
          ,{bal0|posy=bal0.posy+boxp.hei}
          )
        _->(0,bal0)
    nxtset=Maybe.withDefault set0 (List.head(List.filter (\x->x.index==nxtsetindex) setlst))
  in
  {model
    |bal=bal1
    ,blk=
      if model.curset.index==0 then
        model.blk
      else
        nxtset.initblk
    ,curset=nxtset
    ,reuse=Maybe.withDefault scene0 (Array.get nxtset.scenetyp scenearr)
  }
scene0:Svg Msg
scene0=
  defs
    []
    [ g
        [Svg.Attributes.id("reusebkg")]
        [ rect
            [Svg.Attributes.x("0")
            ,Svg.Attributes.y("0")
            ,Svg.Attributes.width("416")
            ,Svg.Attributes.height("416")
            ,Svg.Attributes.fill("#000000")
            ,Svg.Attributes.stroke("#000000")
            ,Svg.Attributes.strokeWidth("0")
            ]
            []
        ]
     , g
          [Svg.Attributes.id("reuse0")]
          [ image
              [Svg.Attributes.xlinkHref("res/wall1.png")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
     , g
        [Svg.Attributes.id("reuse1")]
        [ image
            [Svg.Attributes.xlinkHref("res/t10.png")--("res/t11.png")
            ,Svg.Attributes.x("0")
            ,Svg.Attributes.y("0")
            ,Svg.Attributes.width("32")
            ,Svg.Attributes.height("32")
            ]
            []
        ]
    , g
        [Svg.Attributes.id("reuse2")]
        [ image
            [Svg.Attributes.xlinkHref("res/t20.gif")--("res/t21.gif")
            ,Svg.Attributes.x("0")
            ,Svg.Attributes.y("0")
            ,Svg.Attributes.width("32")
            ,Svg.Attributes.height("32")
            ]
            []
        ]
    , g
        [Svg.Attributes.id("reuse7")]
        [ image
            [Svg.Attributes.xlinkHref("res/t30.png")--("res/t31.png")
            ,Svg.Attributes.x("0")
            ,Svg.Attributes.y("0")
            ,Svg.Attributes.width("32")
            ,Svg.Attributes.height("32")
            ]
            []
        ]
    , g
        [Svg.Attributes.id("reuse4")]
        [ image
            [Svg.Attributes.xlinkHref("res/t40.png")--("res/t41.gif")
            ,Svg.Attributes.x("0")
            ,Svg.Attributes.y("0")
            ,Svg.Attributes.width("32")
            ,Svg.Attributes.height("32")
            ]
            []
        ]
    ]
scenelst:List(Svg Msg)
scenelst=
  [scene0
  , defs--1:grassfield
      []
      [ g
          [Svg.Attributes.id("reusebkg")]
          [ image
              [Svg.Attributes.xlinkHref("res/bkg1.png")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("416")
              ,Svg.Attributes.height("416")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse0")]
          [ image
              [Svg.Attributes.xlinkHref("res/wall1.png")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse1")]
          [ image
              [Svg.Attributes.xlinkHref("res/box1.png")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse2")]
          [ image
              [Svg.Attributes.xlinkHref("res/monster1.gif")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse3")]
          [ image
              [Svg.Attributes.xlinkHref("res/trap1.gif")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse4")]
          [ image
              [Svg.Attributes.xlinkHref("res/buff.gif")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse5")]
          [ image
              [Svg.Attributes.xlinkHref("res/watermelon.gif")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse6")]
          [ image
              [Svg.Attributes.xlinkHref("res/bonus.gif")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse8")]
          [ image
              [Svg.Attributes.xlinkHref("res/wall1.png")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      ]
  , defs--2:beach
      []
      [ g
          [Svg.Attributes.id("reusebkg")]
          [ image
              [Svg.Attributes.xlinkHref("res/bkg2.png")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("416")
              ,Svg.Attributes.height("416")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse0")]
          [ image
              [Svg.Attributes.xlinkHref("res/wall2.png")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse1")]
          [ image
              [Svg.Attributes.xlinkHref("res/box1.png")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse2")]
          [ image
              [Svg.Attributes.xlinkHref("res/monster2.gif")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse3")]
          [ image
              [Svg.Attributes.xlinkHref("res/trap1.gif")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse4")]
          [ image
              [Svg.Attributes.xlinkHref("res/buff.gif")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse5")]
          [ image
              [Svg.Attributes.xlinkHref("res/watermelon.gif")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse6")]
          [ image
              [Svg.Attributes.xlinkHref("res/bonus.gif")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse8")]
          [ image
              [Svg.Attributes.xlinkHref("res/ocean.png")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      ]
  , defs--3:dungeon
      []
      [ g
          [Svg.Attributes.id("reusebkg")]
          [ image
              [Svg.Attributes.xlinkHref("res/bkg3.png")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("416")
              ,Svg.Attributes.height("416")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse0")]
          [ image
              [Svg.Attributes.xlinkHref("res/wall3.png")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse1")]
          [ image
              [Svg.Attributes.xlinkHref("res/box1.png")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse2")]
          [ image
              [Svg.Attributes.xlinkHref("res/monster3.png")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse3")]
          [ image
              [Svg.Attributes.xlinkHref("res/trap1.gif")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse4")]
          [ image
              [Svg.Attributes.xlinkHref("res/buff.gif")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse5")]
          [ image
              [Svg.Attributes.xlinkHref("res/watermelon.gif")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse6")]
          [ image
              [Svg.Attributes.xlinkHref("res/bonus.gif")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse8")]
          [ image
              [Svg.Attributes.xlinkHref("res/wall3.png")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      ]
  , defs--4:truend_on_beach
      []
      [ g
          [Svg.Attributes.id("reusebkg")]
          [ image
              [Svg.Attributes.xlinkHref("res/bkg2.png")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("416")
              ,Svg.Attributes.height("416")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse0")]
          [ image
              [Svg.Attributes.xlinkHref("res/ocean.png")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse1")]
          [ image
              [Svg.Attributes.xlinkHref("res/box1.png")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse2")]
          [ image
              [Svg.Attributes.xlinkHref("res/monster2.gif")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse3")]
          [ image
              [Svg.Attributes.xlinkHref("res/trap1.gif")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse4")]
          [ image
              [Svg.Attributes.xlinkHref("res/buff.gif")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse5")]
          [ image
              [Svg.Attributes.xlinkHref("res/watermelon.gif")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse6")]
          [ image
              [Svg.Attributes.xlinkHref("res/bonus.gif")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      , g
          [Svg.Attributes.id("reuse8")]
          [ image
              [Svg.Attributes.xlinkHref("res/truend.gif")
              ,Svg.Attributes.x("0")
              ,Svg.Attributes.y("0")
              ,Svg.Attributes.width("32")
              ,Svg.Attributes.height("32")
              ]
              []
          ]
      ]
  ]
scenearr:Array(Svg Msg)
scenearr=Array.fromList scenelst
wallconteff:BlockProperty
wallconteff=
  {typ=0--wall
  ,reflect=True
  ,deltsta=0
  ,deltbalsta=(\x->zerobalsta)
  }
blkcontefflst:List BlockProperty
blkcontefflst=
  [wallconteff
  , {typ=1--box
    ,reflect=True
    ,deltsta=-1
    ,deltbalsta=(\x->zerobalsta)
    }
  , {typ=2--monster
    ,reflect=True
    ,deltsta=-1
    ,deltbalsta=(\x->{zerobalsta|health=-10.0,val=25.0})
    }
  , {typ=3--trap
    ,reflect=False
    ,deltsta=0
    ,deltbalsta=(\x->{zerobalsta|health=-0.2*x})
    }
  , {typ=4--buff
    ,reflect=False
    ,deltsta=-1
    ,deltbalsta=(\x->{zerobalsta|val=5.0,buffcont=2500.0})
    }
  , {typ=5--watermelon
    ,reflect=False
    ,deltsta=-1
    ,deltbalsta=(\x->{zerobalsta|health=75.0})
    }
  , {typ=6--bonus
    ,reflect=False
    ,deltsta=-1
    ,deltbalsta=(\x->{zerobalsta|health=1000.0,val=150.0})
    }
  , {typ=7--for_boss
    ,reflect=True
    ,deltsta=-1
    ,deltbalsta=(\x->{zerobalsta|health=-200.0})
    }
  , {typ=8--unbreakable
    ,reflect=True
    ,deltsta=0
    ,deltbalsta=(\x->zerobalsta)
    }
  ]
blkconteffarr:Array BlockProperty
blkconteffarr=Array.fromList blkcontefflst
chkthisblk:Ball->Float->Block->{bal:Ball,thisblk:Block,balcont:Int,deltbalsta:BallState}
--无接触balcont=0_有接触有碰撞balcont=1_有接触无碰撞balcont=2
chkthisblk bal1 tt thisblk=
  if (bal1.posx+balp.r<thisblk.posx)||(bal1.posx-balp.r>thisblk.posx+blkp.wid)||(bal1.posy<thisblk.posy-balp.r)||(bal1.posy>thisblk.posy+blkp.hei+balp.r) then
    {bal=bal1,thisblk=thisblk,balcont=0,deltbalsta=zerobalsta}
  else
    let
      thisblkconteff=Maybe.withDefault wallconteff (Array.get thisblk.typ blkconteffarr)
    in
    { bal=
        if thisblkconteff.reflect then
          let
            (thisblkmidx,thisblkmidy)=((thisblk.posx+blkp.wid/2),(thisblk.posy+blkp.hei/2))
            (tx1,ty1)=(bal1.posx-thisblkmidx,bal1.posy-thisblkmidy)
          in
          if((abs tx1)<(abs ty1))then
            let
              (posy1,spdy1)=
                if (bal1.posy<thisblkmidy) then
                  (thisblk.posy-balp.r,-(abs(bal1.spdy)))
                else
                  (thisblk.posy+blkp.hei+balp.r,(abs(bal1.spdy)))
            in
            {bal1|posy=posy1,spdy=spdy1}
          else
            let
              (posx1,spdx1)=
                if (bal1.posx<thisblkmidx) then
                  (thisblk.posx-balp.r,-(abs(bal1.spdx)))
                else
                  (thisblk.posx+blkp.wid+balp.r,(abs(bal1.spdx)))
            in
            {bal1|posx=posx1,spdx=spdx1}
        else
          bal1
    ,thisblk={thisblk|sta=thisblk.sta+thisblkconteff.deltsta}
    , balcont=
        if thisblkconteff.reflect then
          1
        else
          2
    ,deltbalsta=thisblkconteff.deltbalsta tt
    }
chkthisblkpenetrate:Ball->Float->Block->{bal:Ball,thisblk:Block,balcont:Int,deltbalsta:BallState}
--无接触balcont=0_有接触有碰撞balcont=1_有接触无碰撞balcont=2
chkthisblkpenetrate bal1 tt thisblk=
  if (bal1.posx+balp.r<thisblk.posx)||(bal1.posx-balp.r>thisblk.posx+blkp.wid)||(bal1.posy<thisblk.posy-balp.r)||(bal1.posy>thisblk.posy+blkp.hei+balp.r) then
    {bal=bal1,thisblk=thisblk,balcont=0,deltbalsta=zerobalsta}
  else if thisblk.typ==8 then
    chkthisblk bal1 tt thisblk
  else
    let
      thisblkconteff=Maybe.withDefault wallconteff (Array.get thisblk.typ blkconteffarr)
      deltbalsta0=thisblkconteff.deltbalsta tt
    in
    {bal=bal1,thisblk={thisblk|sta=0},balcont=2,deltbalsta={deltbalsta0|health=Basics.max 0.0 deltbalsta0.health}}
truendchkthisblk:Ball->Float->Block->{bal:Ball,thisblk:Block,balcont:Int,deltbalsta:BallState}
--无接触balcont=0_有接触有碰撞balcont=1_有接触无碰撞balcont=2
truendchkthisblk bal1 tt thisblk=
  if (bal1.posx+balp.r<thisblk.posx)||(bal1.posx-balp.r>thisblk.posx+blkp.wid)||(bal1.posy<thisblk.posy-balp.r)||(bal1.posy>thisblk.posy+blkp.hei+balp.r) then
    {bal=bal1,thisblk=thisblk,balcont=0,deltbalsta=zerobalsta}
  else if thisblk.typ==8 then
    if (bal1.posx<thisblk.posx)||(bal1.posx>thisblk.posx+blkp.wid)||(bal1.posy<thisblk.posy)||(bal1.posy>thisblk.posy+blkp.hei) then
      {bal=bal1,thisblk=thisblk,balcont=0,deltbalsta=zerobalsta}
    else
      {bal={bal1|spdx=0,spdy=0},thisblk=thisblk,balcont=1,deltbalsta=zerobalsta}
  else
    chkthisblk bal1 tt thisblk
truendchkblk:Ball->List Block->BallState->Float->{bal:Ball,blk:List Block,balsta:BallState,balcont:Bool}
truendchkblk bal1 blk balsta0 tt=
  let
    lst1=List.map (truendchkthisblk bal1 tt) blk
    lst3=List.filter (\x->x.balcont==1) lst1
    (bal2,balcont1)=
      case (List.head lst3) of
        Just res1->(res1.bal,True)
        Nothing->(bal1,False)
  in
  {bal=bal2
  ,blk=blk
  ,balsta=balsta0
  ,balcont=balcont1
  }
