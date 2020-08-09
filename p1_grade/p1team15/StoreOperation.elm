module StoreOperation exposing (..)

import Animation
import Global
import Shapes exposing (Color,Shape,Paddle,Ball, GeneralID(..), Bullet)
import Model exposing (Model, GameMode(..))
import Messages exposing (Msg(..), ShopMsg(..), WinResultMsg(..), LoseResultMsg(..))
import  PreRender

buyIntensify : Model -> ShopMsg -> Model
buyIntensify model stmsg =
    let
         shopstate= model.shopstate
    in
    if model.state==Global.Shop then
    case stmsg of
        StoreBack ->
            let
                nextmodel = PreRender.renderSelectLevel {model|state=Global.ChooseLevel}
                ptext= (PreRender.renderShopLabel nextmodel 3 ) ++ [PreRender.renderMoney nextmodel 3]
                nexttext= Animation.replacetexts nextmodel.viewtext ptext

            in

                {nextmodel| viewtext=nexttext}
        BuyPower ->
            if model.shopstate.power==5 then
                model
            else if model.money < Global.coinsConsumption (model.shopstate.power+1) then
                            PreRender.renderLackMoney {model|state=Global.ShopLackMoney}
            else
                let
                     model1= {model|money= model.money - Global.coinsConsumption (model.shopstate.power+1),shopstate= {shopstate| power= shopstate.power+1 }}
                     ptext= List.drop 0  (PreRender.renderShopLabel model1 2 )
                            |> List.take 1
                     newtext = Animation.replacetext model.viewtext (PreRender.renderMoney model1 2)
                     nexttext=Animation.replacetexts  newtext ptext

                in

                {model1|
                viewimage= model.viewimage ++[PreRender.rendersingleShopBar model 1 (shopstate.power+1)],
                viewtext= nexttext}
        BuyLength ->
            if model.shopstate.length==5 then
                model
            else if model.money < Global.coinsConsumption (model.shopstate.length+1) then
                            PreRender.renderLackMoney {model|state=Global.ShopLackMoney}
            else
                                let
                                     model1= {model|money= model.money - Global.coinsConsumption (model.shopstate.length+1),shopstate= {shopstate| length= shopstate.length+1 }}
                                     ptext= List.drop 1  (PreRender.renderShopLabel model1 2 )
                                            |> List.take 1
                                     newtext = Animation.replacetext model.viewtext (PreRender.renderMoney model1 2)
                                     nexttext=Animation.replacetexts  newtext ptext
                                in
                                {model1|
                                viewimage= model.viewimage ++[PreRender.rendersingleShopBar model 2 (shopstate.length+1)],
                                viewtext= nexttext}
        BuySpeed ->
            if model.shopstate.speed==5 then
                model
            else if model.money < Global.coinsConsumption (model.shopstate.speed+1) then
                            PreRender.renderLackMoney {model|state=Global.ShopLackMoney}
            else
                                let
                                     model1= {model|money= model.money - Global.coinsConsumption (model.shopstate.speed+1),shopstate= {shopstate| speed= shopstate.speed+1 }}
                                     ptext= List.drop 2  (PreRender.renderShopLabel model1 2 )
                                            |> List.take 1
                                     newtext = Animation.replacetext model.viewtext (PreRender.renderMoney model1 2)
                                     nexttext=Animation.replacetexts  newtext ptext
                                in
                                {model1|
                                viewimage= model.viewimage ++[PreRender.rendersingleShopBar model 3 (shopstate.speed+1)],
                                viewtext= nexttext}
        BuyDuration ->
            if model.shopstate.duration==5 then
                model
            else if model.money < Global.coinsConsumption (model.shopstate.duration+1) then
                            PreRender.renderLackMoney {model|state=Global.ShopLackMoney}
            else
                                let
                                     model1= {model|money= model.money - Global.coinsConsumption (model.shopstate.duration+1),shopstate= {shopstate| duration= shopstate.duration+1 }}
                                     ptext= List.drop 3  (PreRender.renderShopLabel model1 2 )
                                            |> List.take 1
                                     newtext = Animation.replacetext model.viewtext (PreRender.renderMoney model1 2)
                                     nexttext=Animation.replacetexts  newtext ptext
                                in
                                {model1|
                                viewimage= model.viewimage ++[PreRender.rendersingleShopBar model 4 (shopstate.duration+1)],
                                viewtext= nexttext}
    else
        model

--
--updatePaddle : ShopMsg -> Paddle -> Paddle
--updatePaddle intensify paddle=
--    case intensify of
--        BuyDuration ->
--            {paddle|duration=paddle.duration+3}
--        BuyLength ->
--            {paddle|length=paddle.length+100}
--        BuySpeed ->
--            {paddle|speed=paddle.speed+0.1}
--        BuyPower ->
--            {paddle|attack=paddle.attack+1}
