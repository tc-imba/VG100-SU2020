module PreRender exposing (..)
import CreateShape
import  Global
import Animation exposing (..)
import Model exposing (Model,GameMode(..))
import Shapes exposing (GeneralID(..))
import Svg.Events
import Messages exposing (Msg(..),WinResultMsg(..),LoseResultMsg(..),SelectMsg(..),ShopMsg(..),LoseMsg(..))
renderSingleButton: Model ->ShapeInfo
renderSingleButton model =
    let
        b1ori=Animation.transforminit (Animation.tupleScale model.size (1.125,0.644)) (0,0)
        b1next=Animation.transforminit (Animation.tupleScale model.size (0.125,0.644)) (0,0)
        b1ani =Animation.AnimateInfo b1ori b1next model.currenttime (model.currenttime+1000) 0 False
        size= Animation.tupleScale model.size (0.18,0.153)
        b1shape={color = {red=0,green=0,blue=0},center= (500,500), radius =50, shapetype = -2,duration = 10000, special=0,size = Just size,angle = 0,id=PaddleID -1,opacity=0}
        lb= Animation.ShapeInfo b1ani b1shape [Svg.Events.onClick (SelectMode 1) ]  Global.ChooseMode (Animation.SelectSingleDoubel 1) 2
    in
        lb

renderDoubleButton: Model ->ShapeInfo
renderDoubleButton model =
    let
        b2ori=Animation.transforminit (Animation.tupleScale model.size (1.875,0.644)) (0,0)
        b2next=Animation.transforminit (Animation.tupleScale model.size (0.875,0.644)) (0,0)
        b2ani =  Animation.AnimateInfo b2ori b2next model.currenttime (model.currenttime+1000) 0 False
        size2= Animation.tupleScale model.size (0.18,0.153)
        b2shape={color = {red=0,green=0,blue=0},center= (500,500), radius =50, shapetype = -2,duration = 10000, special=0,size = Just size2,angle = 0,id=PaddleID -1,opacity=0}
        lb2= Animation.ShapeInfo b2ani b2shape [Svg.Events.onClick (SelectMode 2) ]  Global.ChooseMode (Animation.SelectSingleDoubel 2) 2
    in
        lb2
renderSelectModeBg : Model -> ImageInfo
renderSelectModeBg model =

    let
        ori= Animation.transforminit (Animation.tupleScale model.size (1.5,0.5)) model.size
        modif={ori|opacity=0}
        next=Animation.transforminit (Animation.tupleScale model.size (0.5,0.5)) model.size
        ani=Animation.AnimateInfo modif next model.currenttime (model.currenttime+1000) 0 True
        sd= Animation.ImageInfo ani "assets/singledouble@0.5x.png" [] Global.ChooseMode Animation.ModeSelectBg 2
    in
        sd
renderSelectMode : Model -> Model
renderSelectMode model =
    if model.state == Global.ChooseMode then
        let
                        sd=renderSelectModeBg model
                        newlist =Animation.addImageInfo sd model.viewimage
                                 |>updateImageInfo model.currenttime
                                 |>clearImageInfo sd.sceneID sd.animation.endtime
                        lb= renderSingleButton model
                        lb2= renderDoubleButton model
                        newshape= model.viewshape ++[lb,lb2]
                                |>updateShapeInfo model.currenttime
                                |>clearShapeInfo lb.sceneID lb.animation.endtime
        in
                    {model| viewimage = newlist , viewshape = newshape}
    else
        model
renderaLevel : (Float,Float)->(Float,Float)->Int-> Model->ShapeInfo
renderaLevel pos scale number model =
    let
            ori= Animation.transforminit (Animation.tupleScale model.size pos) (0,0)
            next=Animation.transforminit (Animation.tupleScale model.size pos) (0,0)
            ani =Animation.AnimateInfo ori next (model.currenttime+1000) (model.currenttime+1000) 0 False
            size=Animation.tupleScale model.size scale
            shape={color = {red=0,green=0,blue=0},center= (500,500), radius =(Tuple.second size)/2, shapetype = 0,duration = 10000, special=0,size = Just size,angle = 0,id=PaddleID -1,opacity=0}
            ans= Animation.ShapeInfo ani shape [Svg.Events.onClick ( SelectLevel (ClickLevel number)),Svg.Events.onMouseOver (SelectLevel (ViewLevel number)),Svg.Events.onMouseOut (SelectLevel (LeaveLevel number)) ]  Global.ChooseLevel (Animation.Level number) 1
    in
            ans

renderLevels : Model -> List ShapeInfo
renderLevels model =
    let
        poslist= [(0.05,0.25),(0.16,0.6),(0.4,0.21),(0.45,0.80),(0.8,0.23),(0.75,0.70)]
        scalelist= [(1,1),(1,1),(1,1),(1,1),(1,1),(1,1)]
        --indexlist=  List.range
        --abs = List.map renderaLevel poslist scalelist
        b1 = renderaLevel (0.07,0.29) (0.18,0.178) 1 model
        b2 = renderaLevel (0.18,0.585) (0.19,0.223) 2 model
        b3 = renderaLevel (0.389,0.18) (0.20,0.223) 3 model
        b4 = renderaLevel (0.435,0.76) (0.205,0.267) 4 model
        b5 = renderaLevel (0.795,0.20) (0.21,0.215) 5 model
        b6 = renderaLevel (0.755,0.675) (0.22,0.36) 6 model
    in
        [b1,b2,b3,b4,b5,b6]

renderShopButton : Model -> ShapeInfo
renderShopButton  model =
        let
            b1ori=Animation.transforminit (Animation.tupleScale model.size (1.125,0.644)) (0,0)
            b1next=Animation.transforminit (Animation.tupleScale model.size (0.98,1)) (0,0)
            b1ani =Animation.AnimateInfo b1ori b1next model.currenttime (model.currenttime+1000) 0 False
            size= Animation.tupleScale model.size (0.14,0.48)
            b1shape={color = {red=0,green=0,blue=0},center= (500,500), radius =(Tuple.second size)/2, shapetype = 0,duration = 10000, special=0,size = Just size,angle = 0,id=PaddleID -1,opacity=0}
            lb= Animation.ShapeInfo b1ani b1shape [Svg.Events.onClick (SelectLevel Messages.LeveltoShop)]  Global.ChooseLevel (Animation.LeveltoShop) 2
        in
            lb
renderShopBackButton : Model -> ShapeInfo
renderShopBackButton  model =
        let
            b1ori=Animation.transforminit (Animation.tupleScale model.size (1.125,0.644)) (0,0)
            b1next=Animation.transforminit (Animation.tupleScale model.size (0.95,0.95)) (0,0)
            b1ani =Animation.AnimateInfo b1ori b1next model.currenttime (model.currenttime+1000) 0 False
            size= Animation.tupleScale model.size (0.14,0.17)
            b1shape={color = {red=0,green=0,blue=0},center= (500,500), radius =50, shapetype = -2,duration = 10000, special=0,size = Just size,angle = 0,id=PaddleID -1,opacity=0}
            lb= Animation.ShapeInfo b1ani b1shape [Svg.Events.onClick (Messages.Shop Messages.StoreBack) ]  Global.Shop (Animation.StoreBack) 2
        in
            lb
renderalineShopBar: Model-> Int ->Int->List ImageInfo
renderalineShopBar model line value =
    let
        filename =List.map (\x ->  if x>value then "assets/whiteblock.png" else "assets/blackblock.png") (List.range 1 5)
        size=Animation.tupleScale model.size (0.0556,0.0523)

        pos= List.map (\x-> (0.0637*(toFloat x)+0.3444,0.1556*toFloat(line)+0.18817)) (List.range 1 5)
        ori=List.map (\x ->Animation.transforminit (Animation.tupleScale model.size x) size) pos
        modif=List.map (\x->{x|opacity=0}) ori
        next=List.map (\x ->Animation.transforminit (Animation.tupleScale model.size x) size) pos
        ani=List.map3 (\x y z->Animation.AnimateInfo x y (model.currenttime+900+100*toFloat z) (model.currenttime+900+200+100*toFloat z) 0 False) modif next (List.range 1 5)
        sd=List.map2(\x y ->  Animation.ImageInfo x y [] Global.Shop Animation.ShopBars 2  ) ani   filename
    in
        sd
rendersingleShopBar: Model-> Int ->Int-> ImageInfo
rendersingleShopBar model line column =
    let
        filename = "assets/blackblock.png"
        size=Animation.tupleScale model.size (0.0556,0.0523)
        pos=(0.0637*(toFloat column)+0.3444,0.1556*toFloat(line)+0.18817)
        ori=Animation.transforminit (Animation.tupleScale model.size pos) size
        modif={ori|opacity=0}
        next=Animation.transforminit (Animation.tupleScale model.size pos) size
        ani=Animation.AnimateInfo modif next (model.currenttime) (model.currenttime+100) 0 False
        sd=Animation.ImageInfo ani filename [] Global.Shop Animation.ShopBars 2
    in
        sd

renderShopBar : Model ->List ImageInfo
renderShopBar model =
    let
        answerlist = renderalineShopBar model 1 model.shopstate.power
                     |>(++) (renderalineShopBar model 2 model.shopstate.length)
                     |>(++) (renderalineShopBar model 3 model.shopstate.speed)
                     |>(++) (renderalineShopBar model 4 model.shopstate.duration)

    in
        answerlist


renderShopBuyButton : Model -> List ShapeInfo
renderShopBuyButton model =
    let
        pos= List.map (\x-> (0.793,0.1556*toFloat(x)+0.18817)) (List.range 1 5)

        size=Animation.tupleScale model.size (0.09165,0.0672)

        m =[Messages.Shop Messages.BuyPower,Messages.Shop Messages.BuyLength,Messages.Shop Messages.BuySpeed,Messages.Shop Messages.BuyDuration]
        m2=[Animation.BuyPower,Animation.BuyLength,Animation.BuySpeed,Animation.BuyDuration]
        shape={color = {red=0,green=0,blue=0},center= (500,500), radius =1, shapetype = -2,duration = 10000, special=0,size = Just size,angle = 0,id=PaddleID -1,opacity=0}
        ori=List.map (\x ->Animation.transforminit (Animation.tupleScale model.size x) (0,0)) pos
        modif=List.map (\x->{x|opacity=0}) ori
        next=List.map (\x ->Animation.transforminit (Animation.tupleScale model.size x) (0,0)) pos
        ani=List.map3 (\x y z->Animation.AnimateInfo x y (model.currenttime) (model.currenttime) 0 False) modif next (List.range 1 4)
        sd=List.map3(\x y z->  Animation.ShapeInfo x shape [(Svg.Events.onClick y)] Global.Shop z 1  ) ani m m2
    in
        sd
renderMoney : Model->Int ->TextInfo
renderMoney model x=
    let

                (ori,next)= case x of
                                1-> (Animation.transforminit (Animation.tupleScale model.size (0.793,1.1769)) model.size, Animation.transforminit (Animation.tupleScale model.size (0.793,0.1769)) model.size)
                                2->(Animation.transforminit (Animation.tupleScale model.size (0.793,0.1769)) model.size, Animation.transforminit (Animation.tupleScale model.size (0.793,0.1769)) model.size)
                                _->(Animation.transforminit (Animation.tupleScale model.size (0.793,0.1769)) model.size, Animation.transforminit (Animation.tupleScale model.size (0.793,0.1769)) model.size)
                (modori,modnext)=case x of
                                    1->({ori|opacity=0},next)
                                    2->(ori,next)
                                    3-> (ori,{next| opacity=0})
                                    _->(ori,next)
                time = case x of
                        1-> model.currenttime+1000
                        2->model.currenttime
                        3->model.currenttime+200
                        _->model.currenttime+1000
                deletetime = case x of
                        3->200
                        _->0
                ani=Animation.AnimateInfo modori modnext model.currenttime time deletetime True
                sd= Animation.TextInfo ani (String.fromInt model.money) 30 (1500,750) (Shapes.Color 255 255 0)  Global.Shop (Animation.ShopMoney) 2
    in
                sd

renderShopLabel : Model->Int ->List TextInfo
renderShopLabel model x=
    let
                text= Global.getMoneyList model.shopstate
                      |> List.map (\a -> if a <0 then "MAXIMUM" else String.fromInt a++ " Coins")
                base= List.map(\a-> case x of
                                1-> (Animation.transforminit (Animation.tupleScale model.size (0.793,0.1556*toFloat(a)+1.18817)) model.size, Animation.transforminit (Animation.tupleScale model.size (0.793,0.1556*toFloat(a)+0.18817)) model.size)
                                2->(Animation.transforminit (Animation.tupleScale model.size (0.793,0.1556*toFloat(a)+0.18817)) model.size, Animation.transforminit (Animation.tupleScale model.size (0.793,0.1556*toFloat(a)+0.18817)) model.size)
                                _->(Animation.transforminit (Animation.tupleScale model.size (0.793,0.1556*toFloat(a)+0.18817)) model.size, Animation.transforminit (Animation.tupleScale model.size (0.793,0.1556*toFloat(a)+0.18817)) model.size)
                              ) (List.range 1 4)
                modifyed=List.map (\(ori,next)->case x of
                                                1->({ori|opacity=0},next)
                                                2->(ori,next)
                                                3-> (ori,{next| opacity=0})
                                                _->(ori,next)
                                    ) base
                time = case x of
                        1-> model.currenttime+1000
                        2->model.currenttime
                        3->model.currenttime+200
                        _->model.currenttime+1000
                deletetime = case x of
                        3->200
                        _->0
                ani=List.map (\(modori,modnext)->Animation.AnimateInfo modori modnext model.currenttime time deletetime True) modifyed
                ml=[Animation.PowerMoney,Animation.LengthMoney,Animation.SpeedMoney,Animation.DurationMoney]
                sd=List.map3(\a y z-> Animation.TextInfo a y 18 (1500,750) (Shapes.Color 255 255 0)  Global.Shop z 2) ani text ml
    in
                sd


renderShopBg : Model -> ImageInfo
renderShopBg model =
    let
            ori= Animation.transforminit (Animation.tupleScale model.size (0.5,1.5)) model.size
            modif={ori|opacity=0}
            next=Animation.transforminit (Animation.tupleScale model.size (0.5,0.5)) model.size
            ani=Animation.AnimateInfo modif next model.currenttime (model.currenttime+1000) 0 True
            sd= Animation.ImageInfo ani "assets/shop@0.5x.png" [] Global.Shop (Animation.ShopBg) 2
        in
            sd
renderShop : Model -> Model
renderShop model  =
    if model.state == Global.Shop then
            let
                            sd=renderShopBg model
                            newlist =( renderShopBar model)
                                     |>(++) (Animation.addImageInfo sd model.viewimage)
                                     |>updateImageInfo model.currenttime
                                     |>clearImageInfo sd.sceneID sd.animation.endtime

                            lbshop=renderShopBackButton model
                            newshape= model.viewshape ++[lbshop]
                                    |>(++) (renderShopBuyButton model)
                                    |>updateShapeInfo model.currenttime
                                    |>clearShapeInfo lbshop.sceneID lbshop.animation.endtime
                            newtext =List.filter (\x->x.textID /= Animation.ShopMoney) model.viewtext
                                    |> (++) ([renderMoney model 1]++renderShopLabel model 1)
            in
                        {model| viewimage = newlist , viewshape = newshape,viewtext= newtext}
        else
            model
renderLackBg : Model->ImageInfo
renderLackBg model =
    let
                ori= Animation.transforminit (Animation.tupleScale model.size (0.5,0.5)) (Animation.tupleScale model.size (0.03,0.03))
                modif={ori|opacity=0}
                next=Animation.transforminit (Animation.tupleScale model.size (0.5,0.5)) (Animation.tupleScale model.size (0.3,0.3))
                ani=Animation.AnimateInfo modif next model.currenttime (model.currenttime+500) 0 False
                sd= Animation.ImageInfo ani "assets/moneylack.png" [] Global.ResumeLackMoney (Animation.LackMoneyBanner) 0
    in
                sd
renderLackBgdelete : Model->ImageInfo
renderLackBgdelete model =
    let
                ori= Animation.transforminit (Animation.tupleScale model.size (0.5,0.5)) (Animation.tupleScale model.size (0.3,0.3))
                modif={ori|opacity=0}
                next=Animation.transforminit (Animation.tupleScale model.size (0.5,0.5)) (Animation.tupleScale model.size (0.03,0.03))
                ani=Animation.AnimateInfo modif next model.currenttime (model.currenttime+200) (model.currenttime+200) False
                sd= Animation.ImageInfo ani "assets/moneylack.png" [] Global.ResumeLackMoney (Animation.LackMoneyBanner) 0
    in
                sd
renderLackOkButton : Model -> ShapeInfo
renderLackOkButton model =
    let
                b1ori=Animation.transforminit (Animation.tupleScale model.size (0.5,0.573)) (0,0)
                b1next=Animation.transforminit (Animation.tupleScale model.size (0.5,0.573)) (0,0)
                b1ani =Animation.AnimateInfo b1ori b1next model.currenttime model.currenttime 0 False
                size= Animation.tupleScale model.size (0.111,0.0779)
                b1shape={color = {red=0,green=0,blue=0},center= (500,500), radius =(Tuple.second size)/2, shapetype = -2,duration = 10000, special=0,size = Just size,angle = 0,id=PaddleID -1,opacity=0}
                lb= Animation.ShapeInfo b1ani b1shape [Svg.Events.onClick (Messages.LackMoneyOK)]  Global.ShopLackMoney (Animation.LackMoneyOK) 0
     in
                lb

renderLackMoney : Model->Model
renderLackMoney model =
    if model.state == Global.ResumeLackMoney || model.state == Global.ShopLackMoney then
                let
                                sd=renderLackBg model
                                newlist =(Animation.addImageInfo sd model.viewimage)
                                newshape= [renderLackOkButton model]++model.viewshape
                in
                            {model| viewimage = newlist , viewshape = newshape}
            else
                model
renderSelectLevelBg : Model -> ImageInfo
renderSelectLevelBg model =
    let
            ori= Animation.transforminit (Animation.tupleScale model.size (0.5,1.5)) model.size
            modif={ori|opacity=0}
            next=Animation.transforminit (Animation.tupleScale model.size (0.5,0.5)) model.size
            ani=Animation.AnimateInfo modif next model.currenttime (model.currenttime+1000) 0 True
            sd= Animation.ImageInfo ani "assets/choose@0.5x.png" [] Global.ChooseLevel Animation.LevelSelectBg 2
        in
            sd


renderLevelDescription : Model ->Int->Int-> ImageInfo
renderLevelDescription model level t =
    let
        poslsit = [(0.327,0.396),(0.445,0.5),(0.6555,0.2708),(0.7062,0.673),(0.5378,0.3086),(0.45551,0.7299)]
        pos =List.head (List.drop (level - 1) poslsit)
            |>Maybe.withDefault (0,0)
        filepath = "assets/level"++ (String.fromInt level  )++ ".png"
        size= Animation.tupleScale model.size (0.4,0.4)
        ori= Animation.transforminit (Animation.tupleScale model.size pos) size
        modif= if t==1 then {ori|opacity=0,scale=(1,0.01)} else {ori|opacity=1,scale=(1,1)}
        next= if t==1 then {ori|opacity=1,scale=(1,1)} else {ori|opacity=0,scale=(1,0.01)}
        ani= if t==1 then Animation.AnimateInfo modif next model.currenttime (model.currenttime+300) 0 False else Animation.AnimateInfo modif next model.currenttime (model.currenttime+300) (model.currenttime+300) False
        sd= Animation.ImageInfo ani filepath [] Global.ChooseLevel (Animation.LevelIntro level) 1
    in
        sd


renderSelectLevel : Model -> Model
renderSelectLevel model  =
    if model.state == Global.ChooseLevel  then
            let
                            sd=renderSelectLevelBg model
                            newlist =Animation.addImageInfo sd model.viewimage
                                     |>updateImageInfo model.currenttime
                                     |>clearImageInfo sd.sceneID sd.animation.endtime

                            lb=renderLevels model
                            lbshop=renderShopButton model
                            newshape= model.viewshape ++lb
                                    |>updateShapeInfo model.currenttime
                                    |>clearShapeInfo sd.sceneID sd.animation.endtime
                            newshape1=newshape++[lbshop]
                                |>updateShapeInfo model.currenttime
                                |>clearShapeInfo lbshop.sceneID lbshop.animation.endtime
            in
                        {model| viewimage = newlist , viewshape = newshape1, viewtext= renderbest model 1}
        else
            model

renderGameBg : Model -> Int -> ImageInfo
renderGameBg model x=
        let
                ori= Animation.transforminit (Animation.tupleScale model.size (1.0,1.0)) model.size
                modif={ori|opacity=0}
                next=Animation.transforminit (Animation.tupleScale model.size (0.5,0.5)) model.size
                ani=Animation.AnimateInfo modif next model.currenttime (model.currenttime+1000) 0 True
                sd= Animation.ImageInfo ani "assets/mmexport1591954198867@0.5x.png" [] (Global.Game x) Animation.GameBackground 2
        in
                sd
renderGame : Model -> Int -> Model
renderGame model x=
    if model.state == (Global.Game x) then
                let
                                sd=renderGameBg model x
                                newlist =Animation.addImageInfo sd model.viewimage
                                         |>updateImageInfo model.currenttime
                                         |>clearImageInfo sd.sceneID sd.animation.endtime
                                newshape = model.viewshape
                                            |> clearShapeInfo sd.sceneID sd.animation.endtime
                                            |> updateShapeInfo model.currenttime
                in
                            {model| viewimage = newlist ,viewshape= newshape,viewtext= model.viewtext ++ (renderMsg model 1)}
            else
                model

renderWinIc : Model -> ImageInfo
renderWinIc model =
     let
             ori= Animation.transforminit (Animation.tupleScale model.size (0.5,1.5)) model.size
             modif={ori|opacity=0}
             next=Animation.transforminit (Animation.tupleScale model.size (0.5,0.5)) model.size
             ani=Animation.AnimateInfo modif next model.currenttime (model.currenttime+1000) (model.currenttime+2000) False
             sd= Animation.ImageInfo ani "assets/win@0.5x.png" [] Global.Win Animation.WinIcon 1
         in
             sd

renderWinBg : Model -> ImageInfo
renderWinBg model =
     let
             ori= Animation.transforminit (Animation.tupleScale model.size (0.5,1.5)) model.size
             modif={ori|opacity=0}
             next=Animation.transforminit (Animation.tupleScale model.size (0.5,0.5)) model.size
             ani=Animation.AnimateInfo modif next (model.currenttime+2000) (model.currenttime+3000) 0 True
             sd= Animation.ImageInfo ani "assets/winresult.png" [] Global.WinResult Animation.WinResultBg 2
     in
             sd

renderWinRetry : Model -> ShapeInfo
renderWinRetry model =
        let
            b1ori=Animation.transforminit (Animation.tupleScale model.size (1.125,0.644)) (0,0)
            b1next=Animation.transforminit (Animation.tupleScale model.size (0.34,0.83)) (0,0)
            b1ani =Animation.AnimateInfo b1ori b1next (model.currenttime+2000) (model.currenttime+3000) 0 False
            size= Animation.tupleScale model.size (0.041,0.112)
            b1shape={color = {red=0,green=0,blue=0},center= (500,500), radius =(Tuple.first size), shapetype = 0,duration = 10000, special=0,size = Just size,angle = 0,id=PaddleID -1,opacity=0}
            lb= Animation.ShapeInfo b1ani b1shape [Svg.Events.onClick (WinResult Messages.WinRetry) ]  Global.WinResult Animation.WinMenu  2
        in
            lb
renderWinMenu : Model -> ShapeInfo
renderWinMenu model =
        let
            b1ori=Animation.transforminit (Animation.tupleScale model.size (1.125,0.644)) (0,0)
            b1next=Animation.transforminit (Animation.tupleScale model.size (0.497,0.83)) (0,0)
            b1ani =Animation.AnimateInfo b1ori b1next (model.currenttime+2000) (model.currenttime+3000) 0 False
            size= Animation.tupleScale model.size (0.041,0.112)
            b1shape={color = {red=0,green=0,blue=0},center= (500,500), radius =(Tuple.first size), shapetype = 0,duration = 10000, special=0,size = Just size,angle = 0,id=PaddleID -1,opacity=0}
            lb= Animation.ShapeInfo b1ani b1shape [Svg.Events.onClick (WinResult Messages.WinMenu) ]  Global.WinResult Animation.WinMenu  2
        in
            lb
renderWinNext : Model -> ShapeInfo
renderWinNext model =
        let
            b1ori=Animation.transforminit (Animation.tupleScale model.size (1.125,0.644)) (0,0)
            b1next=Animation.transforminit (Animation.tupleScale model.size (0.665,0.83)) (0,0)
            b1ani =Animation.AnimateInfo b1ori b1next (model.currenttime+2000) (model.currenttime+3000) 0 False
            size= Animation.tupleScale model.size (0.041,0.112)
            b1shape={color = {red=0,green=0,blue=0},center= (500,500), radius =(Tuple.first size), shapetype = 0,duration = 10000, special=0,size = Just size,angle = 0,id=PaddleID -1,opacity=0}
            lb= Animation.ShapeInfo b1ani b1shape [Svg.Events.onClick (WinResult Messages.Next) ]  Global.WinResult Animation.WinMenu  2
        in
            lb
renderWinresult : Model -> Model
renderWinresult model  =
     if model.state == Global.WinResult then
             let
                             sd1=renderWinBg model
                             newlist1 =  model.viewimage ++[sd1]
                                      |>updateImageInfo model.currenttime
                                      |>clearImageInfo sd1.sceneID sd1.animation.endtime
                             sd=renderWinIc model
                             newlist =Animation.addImageInfo sd newlist1
                                      |>updateImageInfo model.currenttime
                             lb1=renderWinRetry model
                             lb2=renderWinMenu model
                             lb3=renderWinNext model

                             t= model.currenttime - model.starttime
                             newshape=model.viewshape++[lb1,lb2,lb3]
                                 |>updateShapeInfo model.currenttime
                                 |>clearShapeInfo sd1.sceneID sd1.animation.endtime
                             newtext = renderbonus model 1 2000
                             bonus=((Global.getLevelSetting model.levelnumber).number - (List.length model.shapes)) *5
             in
                         {model| viewimage = newlist , viewshape = newshape,viewtext= newtext,money= model.money + bonus,best= Global.breakbest model.best model.levelnumber t}
         else
             model

renderLoseIc : Model -> ImageInfo
renderLoseIc model =
     let
             ori= Animation.transforminit (Animation.tupleScale model.size (0.5,1.5)) (Animation.tupleHalf model.size)
             modif={ori|opacity=0}
             next=Animation.transforminit (Animation.tupleScale model.size (0.5,0.5)) (Animation.tupleHalf model.size)
             ani=Animation.AnimateInfo modif next model.currenttime (model.currenttime+1000) 0 True
             sd= Animation.ImageInfo ani "assets/fail.png" [] Global.Lose Animation.LoseBanner 1
         in
             sd
renderLoseOK : Model -> ShapeInfo
renderLoseOK model =
    let
                b1ori=Animation.transforminit (Animation.tupleScale model.size (1.125,0.644)) (0,0)
                b1next=Animation.transforminit (Animation.tupleScale model.size (0.405,0.575)) (0,0)
                b1ani =Animation.AnimateInfo b1ori b1next (model.currenttime+1000) (model.currenttime+1000) 0 False
                size= Animation.tupleScale model.size (0.031,0.112)
                b1shape={color = {red=0,green=0,blue=0},center= (500,500), radius =(Tuple.first size ), shapetype = 0,duration = 10000, special=0,size = Just size,angle = 0,id=PaddleID -1,opacity=0}
                lb= Animation.ShapeInfo b1ani b1shape [Svg.Events.onClick (Messages.LoseWindow Messages.LoseConfirm) ]  Global.Lose Animation.LoseConfirm  1
            in
                lb
renderLoseResume : Model -> ShapeInfo
renderLoseResume model =
    let
                b1ori=Animation.transforminit (Animation.tupleScale model.size (1.125,0.644)) (0,0)
                b1next=Animation.transforminit (Animation.tupleScale model.size (0.585,0.575)) (0,0)
                b1ani =Animation.AnimateInfo b1ori b1next (model.currenttime+1000) (model.currenttime+1000) 0 False
                size= Animation.tupleScale model.size (0.031,0.112)
                b1shape={color = {red=0,green=0,blue=0},center= (500,500), radius =(Tuple.first size ), shapetype = 0,duration = 10000, special=0,size = Just size,angle = 0,id=PaddleID -1,opacity=0}
                lb= Animation.ShapeInfo b1ani b1shape [Svg.Events.onClick (Messages.LoseWindow Messages.LoseResume) ]  Global.Lose Animation.LoseResume  1
            in
                lb
renderLoseSelect : Model-> Model
renderLoseSelect model =
    if model.state== Global.Lose
    then
        let
            ic= renderLoseIc model

            newlist= model.viewimage ++[ic]
            b1= renderLoseOK model
            b2= renderLoseResume model


        in
            {model | viewimage = newlist,viewshape= model.viewshape ++[b1,b2] }
    else
    model

renderLoseBg : Model -> ImageInfo
renderLoseBg model =
     let
             ori= Animation.transforminit (Animation.tupleScale model.size (0.5,1.5)) model.size
             modif={ori|opacity=0}
             next=Animation.transforminit (Animation.tupleScale model.size (0.5,0.5)) model.size
             ani=Animation.AnimateInfo modif next (model.currenttime) (model.currenttime+1000) 0 True
             sd= Animation.ImageInfo ani "assets/ResultLose@0.5x.png" [] Global.WinResult Animation.LoseResultBg 2
     in
             sd

renderLoseRetry : Model -> ShapeInfo
renderLoseRetry model =
        let
            b1ori=Animation.transforminit (Animation.tupleScale model.size (1.125,0.644)) (0,0)
            b1next=Animation.transforminit (Animation.tupleScale model.size (0.34,0.83)) (0,0)
            b1ani =Animation.AnimateInfo b1ori b1next (model.currenttime) (model.currenttime+1000) 0 False
            size= Animation.tupleScale model.size (0.041,0.112)
            b1shape={color = {red=0,green=0,blue=0},center= (500,500), radius =(Tuple.first size), shapetype = 0,duration = 10000, special=0,size = Just size,angle = 0,id=PaddleID -1,opacity=0}
            lb= Animation.ShapeInfo b1ani b1shape [Svg.Events.onClick (LoseResult Messages.LoseRetry) ]  Global.LoseResult Animation.LoseRetry  2
        in
            lb
renderLoseMenu : Model -> ShapeInfo
renderLoseMenu model =
        let
            b1ori=Animation.transforminit (Animation.tupleScale model.size (1.125,0.644)) (0,0)
            b1next=Animation.transforminit (Animation.tupleScale model.size (0.497,0.83)) (0,0)
            b1ani =Animation.AnimateInfo b1ori b1next (model.currenttime) (model.currenttime+1000) 0 False
            size= Animation.tupleScale model.size (0.041,0.112)
            b1shape={color = {red=0,green=0,blue=0},center= (500,500), radius =(Tuple.first size), shapetype = 0,duration = 10000, special=0,size = Just size,angle = 0,id=PaddleID -1,opacity=0}
            lb= Animation.ShapeInfo b1ani b1shape [Svg.Events.onClick (LoseResult Messages.LoseMenu) ]  Global.LoseResult Animation.LoseMenu  2
        in
            lb
renderLoseNext : Model -> ShapeInfo
renderLoseNext model =
        let
            b1ori=Animation.transforminit (Animation.tupleScale model.size (1.125,0.644)) (0,0)
            b1next=Animation.transforminit (Animation.tupleScale model.size (0.665,0.83)) (0,0)
            b1ani =Animation.AnimateInfo b1ori b1next (model.currenttime) (model.currenttime+1000) 0 False
            size= Animation.tupleScale model.size (0.041,0.112)
            b1shape={color = {red=0,green=0,blue=0},center= (500,500), radius =(Tuple.first size), shapetype = 0,duration = 10000, special=0,size = Just size,angle = 0,id=PaddleID -1,opacity=0}
            lb= Animation.ShapeInfo b1ani b1shape [Svg.Events.onClick (LoseResult Messages.ResulttoShop) ]  Global.LoseResult Animation.LosetoShop  2
        in
            lb
renderLoseresult : Model -> Model
renderLoseresult model  =
     if model.state == Global.LoseResult then
             let
                             sd1=renderLoseBg model
                             newlist1 =Animation.addImageInfo sd1 model.viewimage
                                      |>updateImageInfo model.currenttime
                                      |>clearImageInfo sd1.sceneID sd1.animation.endtime
                                      |> List.filter (\x->
                                                        case x.imageID of
                                                            Animation.LoseBanner->False
                                                            Animation.Gamingiamge t-> False
                                                            _-> True
                                                     )
                             lb1=renderLoseRetry model
                             lb2=renderLoseMenu model
                             lb3=renderLoseNext model


                             newshape=model.viewshape++[lb1,lb2,lb3]
                                 |>updateShapeInfo model.currenttime
                                 |>clearShapeInfo lb1.sceneID model.currenttime
                             newtext = renderbonus model 1 0
                             bonus=((Global.getLevelSetting model.levelnumber).number - (List.length model.shapes)) *5
             in
                         {model| viewimage = newlist1 , viewshape = newshape,viewtext = newtext,money= model.money+bonus}
         else
             model
renderpad: Model->Shapes.Paddle->Int-> ShapeInfo
renderpad model pad tp =
    let
                b1ori= if tp==1 then Animation.transforminit (Animation.tupleAdd (Animation.tupleHalf model.size) pad.center) (0,0)
                        else Animation.transforminit pad.center (0,0)
                b1next=Animation.transforminit pad.center (0,0)
                b1ani =if tp==1 then Animation.AnimateInfo b1ori b1next (model.currenttime) (model.currenttime+1000) 0 False
                            else  Animation.AnimateInfo b1ori b1next (model.currenttime) (model.currenttime) 0 False
                b1shape=Shapes.paddletoShape pad
                lb= Animation.ShapeInfo b1ani b1shape []  (Global.Game model.levelnumber) (Animation.Gaming pad.id)  2
            in
                lb

renderashape :Model-> Shapes.Shape -> ShapeInfo
renderashape model shape =
        let
                    b1ori=Animation.transforminit shape.center (0,0)
                    b1next=Animation.transforminit shape.center (0,0)
                    b1ani =Animation.AnimateInfo b1ori b1next (model.currenttime) (model.currenttime) 0 False
                    b1shape={shape|opacity=0.3+0.7*(shape.duration/4)}
                    lb= Animation.ShapeInfo b1ani b1shape []  (Global.Game model.levelnumber) (Animation.Gaming shape.id)  2
                in
                    lb

renderdyingshape : Model -> Shapes.Shape -> ShapeInfo
renderdyingshape model shape =
    let
                        b1ori=Animation.transforminit shape.center (0,0)
                        b1next=Animation.transforminit shape.center (0,0)
                        b1ani =Animation.AnimateInfo b1ori {b1next|scale=(1.2,1.2),opacity=0} (model.currenttime) (model.currenttime+100) (model.currenttime+100) False
                        b1shape={shape|opacity=0.8,color=(Shapes.Color 200 200 255)}
                        lb= Animation.ShapeInfo b1ani b1shape []  (Global.Game model.levelnumber) (Animation.Dying)  2
                    in
                        lb
renderdyingshapes: Model -> List ShapeInfo
renderdyingshapes model =
    List.filter (\x -> (Tuple.second x.center )> CreateShape.recommandradius model.size) model.dyingshape
    |>List.map (\ x-> renderdyingshape model x)

renderOuterShapes : Model -> List ShapeInfo
renderOuterShapes model =
    List.filter (\x -> (Tuple.second x.center )> CreateShape.recommandradius model.size) model.shapes
    |>List.map (\ x-> renderashape model x)



renderaimage : Model->Shapes.Shape  -> ImageInfo
renderaimage model shape  =
     let
             ori= Animation.transforminit shape.center (2*CreateShape.recommandradius model.size,2*CreateShape.recommandradius model.size)
             mori={ori| rotate = (180/ pi * shape.angle)}
             next=Animation.transforminit shape.center (2*CreateShape.recommandradius model.size,2*CreateShape.recommandradius model.size)
             mnext={ori| rotate = (180/ pi * shape.angle)}
             ani=Animation.AnimateInfo mori mnext (model.currenttime) (model.currenttime) 0 False
             sd= Animation.ImageInfo ani ("assets/"++(String.fromInt shape.shapetype)++".png") [] (Global.Game model.levelnumber) (Animation.Gamingiamge shape.id) 1
     in
             sd
renderInnerImage : Model -> List ImageInfo
renderInnerImage model =
    List.filter (\x -> (Tuple.second x.center )> CreateShape.recommandradius model.size) model.shapes
        |>List.map (\ x-> renderaimage model x)

renderBall :  Model ->Int-> ShapeInfo
renderBall model tp =
    let
       b=model.ball
       b1ori=if tp==1 then Animation.transforminit (Animation.tupleAdd (Animation.tupleHalf model.size) b.center) (0,0)
                      else Animation.transforminit b.center (0,0)
       b1next=Animation.transforminit b.center (0,0)
       b1ani =if tp==1 then Animation.AnimateInfo b1ori b1next (model.currenttime) (model.currenttime+1000) 0 False
                    else Animation.AnimateInfo b1ori b1next (model.currenttime) (model.currenttime) 0 False
       b1shape={color = b.color,center= (500,500), radius =b.radius, shapetype = 0,duration = 10000, special=0,size = Nothing,angle = 0,id=PaddleID -1,opacity=1}
       lb= Animation.ShapeInfo b1ani b1shape []  (Global.Game model.levelnumber) (Animation.Gaming BallID)  2
    in
       lb

renderbonus : Model ->Int-> Float -> List TextInfo
renderbonus model tp dt =
    let
                 ori=List.map (\x-> Animation.transforminit (Animation.tupleScale model.size (0.625,x)) (0,0)) [0.374,0.489,0.605]
                 modif=if tp==1 then List.map (\x->{x|opacity=0}) ori else ori
                 next=if tp==2 then List.map (\x->{x|opacity=0}) ori else ori
                 ani= if tp==1 then List.map3 (\x y z->Animation.AnimateInfo x y (model.currenttime+1000 +dt+ (toFloat z)*200) (model.currenttime+1000+dt+(toFloat z)*200+100) 0 False) modif next (List.range 0 2)
                        else List.map3 (\x y z->Animation.AnimateInfo x y (model.currenttime) (model.currenttime) (model.currenttime+300) False) modif next (List.range 0 2)
                 t=model.currenttime - model.starttime
                 str=Global.timetostring t
                 defeat= (Global.getLevelSetting model.levelnumber).number - (List.length model.shapes)
                 slist= [String.fromInt defeat ,str,String.fromInt (defeat*5)]
                 sd=List.map2(\x y-> Animation.TextInfo x y 40 (1500,750)  (Shapes.Color 255 255 255) Global.WinResult Animation.ResultTexts 2) ani slist
    in
                 sd
renderbest : Model ->Int -> List TextInfo
renderbest model tp  =
    let
                 ori=List.map (\x -> Animation.transforminit (Animation.tupleScale model.size x) (0,0))  [(0.07,0.29),(0.18,0.585),(0.389,0.18),(0.435,0.76),(0.795,0.20),(0.755,0.675)]
                 modif=if tp==1 then List.map (\x->{x|opacity=0}) ori else ori
                 next=if tp==2 then List.map (\x->{x|opacity=0}) ori else ori
                 ani= if tp==1 then List.map3 (\x y z->Animation.AnimateInfo x y (model.currenttime+1000 + (toFloat z)*100) (model.currenttime+1000+(toFloat z)*100+200) 0 False) modif next (List.range 0 5)
                        else List.map3 (\x y z->Animation.AnimateInfo x y (model.currenttime) (model.currenttime) (model.currenttime+300) False) modif next (List.range 0 2)
                 slist= List.map (\x->if x /= 1000000 then Global.timetostring x else "--:--") model.best
                 sd=List.map2(\x y-> Animation.TextInfo x y 40 (1500,750)  (Shapes.Color 255 255 255) Global.WinResult Animation.ResultTexts 2) ani slist
    in
                 sd
renderMsg : Model ->Int-> List TextInfo
renderMsg model tp  =
    let
                 ori=List.map (\x-> Animation.transforminit (Animation.tupleScale model.size (0.5,x)) (0,0)) [0.2,0.3,0.4]
                 modif=if tp==1 then List.map (\x->{x|opacity=0}) ori else List.map (\x->{x|opacity=0.5}) ori
                 next=if tp==2 then List.map (\x->{x|opacity=0}) ori else List.map (\x->{x|opacity=0.5}) ori
                 ani= if tp==1 then List.map3 (\x y z->Animation.AnimateInfo x y (model.currenttime+1000 + (toFloat z)*400) (model.currenttime+1000+(toFloat z)*400+400) 0 False) modif next (List.range 0 2)
                        else List.map3 (\x y z->Animation.AnimateInfo x y (model.currenttime) (model.currenttime) (model.currenttime+300) False) modif next (List.range 0 2)
                 slist= ["Press  Enter  to  Start","Press  ESC  to  Quit"] ++ [(if model.gamemode == Model.Single then "Use ↑ ↓ ← → to move" else  "Player1 : WASD    Player2 : ↑ ↓ ← →")]
                 sd=List.map2(\x y-> Animation.TextInfo x y 60 (1500,750)  (Shapes.Color 200 200 200) Global.WinResult Animation.ResultTexts 2) ani slist
    in
                 sd