module Update exposing (..)
import Message exposing (..)
import Game  exposing (..)
import Tuple exposing (..)
import Model as Music exposing (Model(..))
import Random exposing (..)
import List exposing (append, concat, drop, filter, head)
import Random



type alias Point =
               {  x: Float
                , y: Float
               }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Startgame ->
            (Prepare Game.No Music.Play Music.No, Cmd.none)

        Restart ->
            (Start False , Cmd.none)

        Help help ->
            model
            |> showHelp help
            |> saveToStorage

        CRome ->
            Prepare Rome Music.Play Music.Rome
            |> saveToStorage

        CPersia ->
            Prepare Persia Music.Play Music.Persia
            |> saveToStorage

        CGoth ->
            Prepare Goth Music.Play Music.Visigoth
            |> saveToStorage

        Go con->
            case con of
                Rome ->
                    Game rome_init Music.Play
                    |> saveToStorage

                Persia ->
                    Game persia_init Music.Play
                    |> saveToStorage

                Goth ->
                    Game goth_init Music.Play
                    |> saveToStorage

                _ ->
                    model
                    |>saveToStorage

        Firstturn ->
            model
            |> firstenermy
            |> saveToStorage

        Secondturn ->
            model
            |> secondenermy
            |> saveToStorage

        Pause ->
            model
            |> changeStatus Paused
            |> saveToStorage

        Resume ->
            model
            |> changeStatus Playing
            |> saveToStorage

        MoveLeft on ->
            model
                |> boardLeft on
                |> saveToStorage

        MoveRight on ->
            model
                |> boardRight on
                |> saveToStorage

        Tick time ->
                model
                |> animate (min time 25)
                |> checkEnd
                |> saveToStorage

        Gamewin ->
            (End Music.Win, Cmd.none)

        _ ->
            ( model, Cmd.none )


saveToStorage : Model -> ( Model, Cmd Msg )
saveToStorage model =
    ( model, Cmd.none )

showHelp : Bool -> Model -> Model
showHelp help model=
    case model of
        Start h ->
            Start help

        _ ->
            model

firstenermy : Model -> Model
firstenermy model =
    case model of
        Game gamemodel music->
            if gamemodel.con == Rome then
                Game { rome_init |  sta = Playing } music
            else if gamemodel.con == Persia then
                Game { persia_init | sta = Playing } music
            else
                Game { goth_init | sta = Playing } music
        _ ->
            model


secondenermy : Model -> Model
secondenermy model =
    case model of
        Game gamemodel music->
            if gamemodel.con == Rome then
                Game { rome_secd |  sta = Playing } music
            else if gamemodel.con == Persia then
                Game { persia_secd | sta = Playing } music
            else
                Game { goth_secd | sta = Playing } music
        _ ->
            model


changeStatus : Status -> Model -> Model
changeStatus status model=
    case model of
        Game gamemodel music->
            Game { gamemodel | sta = status } music
        _ ->
            model


boardLeft : Bool -> Model -> Model
boardLeft on model =
    case model of
        Game gamemodel music->
            let
                newBoard =
                    Board gamemodel.bod.col gamemodel.bod.vel gamemodel.bod.wid gamemodel.bod.hei gamemodel.bod.x gamemodel.bod.y on False
            in
            Game { gamemodel | bod = newBoard } music
        _ ->
            model


boardRight : Bool -> Model -> Model
boardRight on model =
    case model of
        Game gamemodel music->
            let
                newBoard =
                    Board gamemodel.bod.col gamemodel.bod.vel gamemodel.bod.wid gamemodel.bod.hei gamemodel.bod.x gamemodel.bod.y False on
            in
            Game { gamemodel | bod = newBoard } music
        _ ->
            model


animate: Float -> Model -> Model
animate elapsed model =
    case model of
        Game gamemodel music->
            let
                    (newmodel,ballList)=
                        moveBall (gamemodel, [])
            in
            Game (moveBoard (newmodel)) music
        _ ->
            model


moveBall :  (GameModel, List Ball) -> (GameModel, List Ball)
moveBall (model, newball)=
    let
        ball =
            Maybe.withDefault ball_rome (List.head  model.bal)

        remain =
            List.drop 1 model.bal

        bal_=
            bumpBoard model.bod ball
    in


    let (x_,y_)=
                    (bal_.x+bal_.vel*cos(toFloat(bal_.ang)/180*pi),bal_.y+bal_.vel*sin(toFloat(bal_.ang)/180*pi))
    in


    let ang_=
                if x_ < -model.wid/2 || x_ > model.wid/2 then
                    modBy 360 (180 + -(bal_.ang))
                else
                    modBy 360 (bal_.ang)
    in


    let ang__=
               if y_ >model.hei/2 then
                     modBy 360 -ang_
               else
                     modBy 360 ang_
    in


    let (x__,y__)=
                     (bal_.x+bal_.vel*cos(toFloat(ang__)/180*pi),bal_.y+bal_.vel*sin(toFloat(ang__)/180*pi))
    in


    let bal__=
              Ball bal_.col bal_.vel bal_.rad ang__ x__ y__
    in

    let (lis___, see_, bal___)=
            case model.stp of
                2 ->
                    collideGrid model.lis model.see model.swt model.tur bal__
                _ ->
                    collideGrid model.lis model.see False model.tur bal__
        up =
            case model.stp of
                1 ->
                    3
                2 ->
                    10
                3 ->
                    5
                _ ->
                    5

        (skill, switch) =
            --if List.length newball == 0 then
                if model.ski < 100 && model.swt == False then
                    if List.length(filter (\n -> n.sta == Normal ) model.lis) - List.length(filter (\n -> n.sta == Normal) lis___) > 0 then
                        (model.ski+ up * toFloat (List.length(filter (\n -> n.sta == Normal ) model.lis) - List.length(filter (\n -> n.sta == Normal) lis___)), model.swt)
                    else
                        (model.ski + 1/60 , model.swt)
                else
                    case model.stp of
                        3 ->
                            if model.swt == False && model.ski >= 100 then
                                (100, True)
                            else if model.ski > 0 then
                                (model.ski - 1/6, model.swt)
                            else
                                (0, False)

                        _ ->
                            if List.length(filter (\n -> n.sta == Normal ) model.lis) /= List.length(filter (\n -> n.sta == Normal) lis___)  then
                                (0, False)
                            else
                                (100, True)
           -- else
            --    (model.ski, model.swt)

        bigboard =
            case model.stp of
                3 ->
                    if model.swt then
                        if model.bod.wid < 200 then
                            Board model.bod.col model.bod.vel (1.011*model.bod.wid) model.bod.hei model.bod.x model.bod.y model.bod.left model.bod.right
                        else
                            Board model.bod.col model.bod.vel 200 model.bod.hei model.bod.x model.bod.y model.bod.left model.bod.right
                    else
                        if model.bod.wid > 100 then
                            Board model.bod.col model.bod.vel (0.989*model.bod.wid) model.bod.hei model.bod.x model.bod.y model.bod.left model.bod.right
                        else
                            Board model.bod.col model.bod.vel 100 model.bod.hei model.bod.x model.bod.y model.bod.left model.bod.right
                _ ->
                    model.bod
        fastBall =
            case model.stp of
                3 ->
                    case model.tur of
                        1->
                            if model.swt then
                                if bal___.vel < 45/7 then
                                    Ball bal___.col (1.008*bal___.vel) bal___.rad bal___.ang bal___.x bal___.y
                                else
                                    Ball bal___.col (45/7) bal___.rad bal___.ang bal___.x bal___.y
                            else
                                if bal___.vel > 60/14 then
                                    Ball bal___.col (0.993*bal___.vel) bal___.rad bal___.ang bal___.x bal___.y
                                else
                                    Ball bal___.col (60/14) bal___.rad bal___.ang bal___.x bal___.y
                        _ ->
                            if model.swt then
                                if bal___.vel < 51/7 then
                                    Ball bal___.col (1.003*bal___.vel) bal___.rad bal___.ang bal___.x bal___.y
                                else
                                    Ball bal___.col (51/7) bal___.rad bal___.ang bal___.x bal___.y
                            else
                                if bal___.vel > 85/14 then
                                    Ball bal___.col (0.997*bal___.vel) bal___.rad bal___.ang bal___.x bal___.y
                                else
                                    Ball bal___.col (85/14) bal___.rad bal___.ang bal___.x bal___.y

                _ ->
                    bal___

        (ballList, nswitch , nskill) =
            case model.stp of
                1 ->
                    if fastBall.y > -model.hei then
                        if switch then
                            (List.concat [newball, [fastBall, ball_rome]] , False , 0)
                        else
                            (List.append newball [fastBall], switch , skill)
                    else
                        if switch then
                            (List.concat [newball, [ball_rome]] , False , 0)
                        else
                            (newball, switch , skill)
                _ ->
                    if fastBall.y > -model.hei then
                        (List.append newball [fastBall], switch , skill)
                    else
                        (newball, switch , skill)
    in
        if List.length remain > 0 then
            moveBall ({ model | lis = lis___, bal = remain ,see=see_, ski=nskill, swt=nswitch, bod=bigboard }, ballList)
        else
            ({ model | lis = lis___, bal = ballList ,see=see_, ski=nskill, swt=nswitch, bod=bigboard },ballList)



bumpBoard : Board -> Ball -> Ball
bumpBoard board ball =
    let
        ldis =
            sqrt((ball.x - (board.x - board.wid/2)) ^ 2 + (ball.y - (board.y + board.hei/2)) ^ 2)

        rdis =
            sqrt((ball.x - (board.x + board.wid/2)) ^ 2 + (ball.y - (board.y + board.hei/2)) ^ 2)

        ang_ =
            if ball.x >= board.x - board.wid/2 && ball.x <= board.x + board.wid/2 && ball.ang > 180 then
                if ball.y - ball.rad - board.hei/2 <= board.y && ball.y >= board.y - board.hei/2 - ball.rad then
                    if ball.ang <= 270 && board.right then
                             modBy 360 (340 - ball.ang)
                    else if ball.ang <= 270 && board.left && ball.ang > 210 then
                             modBy 360 (380 - ball.ang)
                    else if ball.ang >270 && board.left then
                             modBy 360 (380 - ball.ang)
                    else if ball.ang >270 && board.right && ball.ang < 330 then
                             modBy 360 (340 - ball.ang)
                    else
                             modBy 360 (360 - ball.ang)
                --else if (ball.x <= board.x - board.wid/2 && ball.x >= board.x - board.wid/2 - ball.rad) ||
                        --(ball.x >= board.x + board.wid/2 && ball.x <= board.x + board.wid/2 + ball.rad) then
                            -- modBy 360 (180 - ball.ang)
                else
                    ball.ang
            else if (ldis <= ball.rad || rdis <= ball.rad) && ball.ang > 180 then
                modBy 360 (360 - ball.ang)
            else
                ball.ang
    in
        {ball | ang=ang_}


collisionBlockWithMinimumCollisionDistance : (List Block) -> Block -> Ball -> Block
collisionBlockWithMinimumCollisionDistance lis_ someBlock bal_ =
    case head lis_ of
        Nothing ->
            someBlock
        Just newBlock ->
            if ((someBlock.x - bal_.x)^2+(someBlock.y - bal_.y)^2<(newBlock.x - bal_.x)^2+(newBlock.y - bal_.y)^2) then
                collisionBlockWithMinimumCollisionDistance (drop 1 lis_) someBlock bal_
            else
                collisionBlockWithMinimumCollisionDistance (drop 1 lis_) newBlock bal_


collideGrid : (List Block) -> Random.Seed -> Bool -> Int -> Ball -> ((List Block), Random.Seed,Ball)
collideGrid lis_  see_ switch level bal_ =
    case head lis_ of
        Nothing->
            ([],see_,bal_)
        Just temp->
            let kind=
                    temp.sha
            in


            let lis_keep=
                    filter (  \someBlock -> ( someBlock.sta==Vanished || (judgeCollideBlock someBlock bal_ 0)==False   ||   cos(pi/180.0*(toFloat (( modBy 360 (-someBlock.ang+round(180/pi* (atan2 (-someBlock.y+bal_.y) (-someBlock.x+bal_.x))))) // round(360/toFloat(kind))) +0.5   )*(360/toFloat(kind))+pi/180.0*(toFloat someBlock.ang))*cos(pi/180*toFloat(bal_.ang))+sin(pi/180.0*(toFloat (( modBy 360 (-someBlock.ang+round(180/pi* (atan2 (-someBlock.y+bal_.y) (-someBlock.x+bal_.x))))) // round(360/toFloat(kind))) +0.5   )*(360/toFloat(kind))+pi/180.0*(toFloat someBlock.ang))*sin(pi/180*toFloat(bal_.ang))>=0  )) lis_
            in


            let lis_collide=
                    filter (  \someBlock -> ( someBlock.sta/=Vanished && (judgeCollideBlock someBlock bal_ 0)==True    &&   cos(pi/180.0*(toFloat (( modBy 360 (-someBlock.ang+round(180/pi* (atan2 (-someBlock.y+bal_.y) (-someBlock.x+bal_.x))))) // round(360/toFloat(kind))) +0.5   )*(360/toFloat(kind))+pi/180.0*(toFloat someBlock.ang))*cos(pi/180*toFloat(bal_.ang))+sin(pi/180.0*(toFloat (( modBy 360 (-someBlock.ang+round(180/pi* (atan2 (-someBlock.y+bal_.y) (-someBlock.x+bal_.x))))) // round(360/toFloat(kind))) +0.5   )*(360/toFloat(kind))+pi/180.0*(toFloat someBlock.ang))*sin(pi/180*toFloat(bal_.ang))<0   )) lis_
            in


            case head lis_collide of
                Nothing->
                    (List.map changeBlockColorAuto lis_, see_, bal_)
                Just someBlock->
                    let collision_=
                            collisionBlockWithMinimumCollisionDistance (drop 1 lis_collide) someBlock bal_
                    in


                    let lis_return=
                            filter(\block->block/=collision_ ) lis_collide
                    in


                    let phi_=
                            round(180/pi* (atan2 (-collision_.y+bal_.y) (-collision_.x+bal_.x)))
                    in


                    let ang_=
                            modBy 360 ( (-bal_.ang+180)+(  2*(   ( modBy 360 (-(collision_.ang)+phi_ )) // round(360/toFloat(kind))     ) +1   )*(round(360/toFloat(kind)))+2*(collision_.ang) )
                    in


                    let bal__=
                            Ball bal_.col bal_.vel bal_.rad ang_ bal_.x bal_.y
                    in

                    let (lis_auto,see__) =
                            if switch then
                                (List.map changeBlockColorAuto (setCollisionBlockOnFire {collision_|sta=Fade}  (append lis_keep lis_return) ),see_)
                            else if collision_.typ>=3 && collision_.typ<6 then
                                (List.map changeBlockColorAuto (append lis_keep ((changeCollisionBlockStatus collision_)::lis_return)),see_)
                            else if collision_.typ>=12 && collision_.typ<15 then
                                (List.map changeBlockColorAuto (append (healThreeBlocks lis_keep level bal__) ((changeCollisionBlockStatus collision_)::lis_return)),see_)
                            else if (collision_.typ>=0 && collision_.typ<3) then
                                (List.map changeBlockColorAuto (setCollisionBlockOnFire {collision_|sta=Darken}  (append lis_keep lis_return) ),see_)
                            else if collision_.typ>=6 && collision_.typ<9 then
                                randomlyChangeBlocksAround (changeCollisionBlockStatus collision_) (append lis_keep lis_return) see_ level bal__
                            else if collision_.typ>=9 && collision_.typ<12 then
                                (List.map changeBlockColorAuto (append lis_keep ({collision_ | typ = collision_.typ - 6 }::lis_return)),see_)
                            else
                                (List.map changeBlockColorAuto (append lis_keep ((changeCollisionBlockStatus collision_)::lis_return)),see_)


                    in

                    ( lis_auto , see__, bal__)


moveBoard : GameModel -> GameModel
moveBoard model =
    let
        x_ =
            if model.bod.right && model.bod.x < model.wid/2 then
                model.bod.x + model.bod.vel
            else if model.bod.left && model.bod.x > -model.wid/2 then
                model.bod.x - model.bod.vel
            else
                model.bod.x

        newBoard =
            Board model.bod.col model.bod.vel model.bod.wid model.bod.hei x_ model.bod.y model.bod.left model.bod.right
    in
        { model | bod = newBoard }


checkEnd : Model -> Model
checkEnd model =
    case model of
        Game gamemodel music->
            if List.length (filter (\ball -> ball.y > gamemodel.bod.y - gamemodel.bod.hei/2) gamemodel.bal) == 0 then
                changeStatus GG model
            else if List.length (filter (\x->x.sta/=Vanished && x.sta/=Darken) gamemodel.lis) == 0 then
                changeStatus Win model
            else
                model
        _ ->
            model


judgeCollideBlock : Block -> Ball -> Int -> Bool
judgeCollideBlock someBlock bal_ i =
                                   let p_0=
                                           Point someBlock.x someBlock.y
                                   in


                                   let p_1=
                                           Point (p_0.x+someBlock.rad*cos(pi/180*toFloat(someBlock.ang)+toFloat(i)*2.0*pi/toFloat(someBlock.sha) ))  (p_0.y+someBlock.rad*sin(pi/180*toFloat(someBlock.ang)+toFloat(i)*2.0*pi/toFloat(someBlock.sha) )      )

                                   in


                                   let p_2=
                                           Point (p_0.x+someBlock.rad*cos(pi/180*toFloat(someBlock.ang)+toFloat(i+1)*2.0*pi/toFloat(someBlock.sha) ))  (p_0.y+someBlock.rad*sin(pi/180*toFloat(someBlock.ang)+toFloat(i+1)*2.0*pi/toFloat(someBlock.sha) )      )

                                   in


                                   let p_3=
                                           Point (p_2.x+bal_.rad*cos(pi/180*toFloat(someBlock.ang)+(toFloat(i)+0.5)*2.0*pi/toFloat(someBlock.sha) )) (p_2.y+bal_.rad*sin(pi/180*toFloat(someBlock.ang)+(toFloat(i)+0.5)*2.0*pi/toFloat(someBlock.sha) ))
                                   in


                                   let p_4=
                                           Point (p_1.x+bal_.rad*cos(pi/180*toFloat(someBlock.ang)+(toFloat(i)+0.5)*2.0*pi/toFloat(someBlock.sha) )) (p_1.y+bal_.rad*sin(pi/180*toFloat(someBlock.ang)+(toFloat(i)+0.5)*2.0*pi/toFloat(someBlock.sha) ))
                                   in


                                   let p_5=
                                           Point ((p_0.x+p_1.x+p_2.x)/3) ((p_0.y+p_1.y+p_2.y)/3)
                                   in


                                   if ( ( ((p_1.y+ -p_2.y)*(bal_.x+ -p_1.x)+ -(p_1.x+ -p_2.x)*(bal_.y+ -p_1.y))  *  ((p_4.y+ -p_3.y)*(bal_.x+ -p_4.x)+ -(p_4.x+ -p_3.x)*(bal_.y+ -p_4.y)) )<=0 && ( ((p_1.y+ -p_4.y)*(bal_.x+ -p_1.x)+ -(p_1.x+ -p_4.x)*(bal_.y+ -p_1.y))  *  ((p_2.y+ -p_3.y)*(bal_.x+ -p_2.x)+ -(p_2.x+ -p_3.x)*(bal_.y+ -p_2.y)) )<=0 ) then
                                       True
                                   else if (( ((p_0.y+ -p_1.y)*(bal_.x+ -p_0.x)+ -(p_0.x+ -p_1.x)*(bal_.y+ -p_0.y))  *  ((p_0.y+ -p_1.y)*(p_5.x+ -p_0.x)+ -(p_0.x+ -p_1.x)*(p_5.y+ -p_0.y)) )>=0 && ( ((p_0.y+ -p_2.y)*(bal_.x+ -p_0.x)+ -(p_0.x+ -p_2.x)*(bal_.y+ -p_0.y))  *  ((p_0.y+ -p_2.y)*(p_5.x+ -p_0.x)+ -(p_0.x+ -p_2.x)*(p_5.y+ -p_0.y)) )>=0 && ( ((p_1.y+ -p_2.y)*(bal_.x+ -p_1.x)+ -(p_1.x+ -p_2.x)*(bal_.y+ -p_1.y))  *  ((p_1.y+ -p_2.y)*(p_5.x+ -p_1.x)+ -(p_1.x+ -p_2.x)*(p_5.y+ -p_1.y)) )>=0) then
                                       True
                                   else if ( (bal_.x+ -p_1.x)^2+(bal_.y+ -p_1.y)^2<=bal_.rad^2) then
                                       True
                                   else if i== someBlock.sha+ -1 then
                                       False
                                   else
                                       judgeCollideBlock someBlock bal_ (i+1)


randomlyChangeBlocksAround : Block->List Block->Random.Seed-> Int ->Ball->(List Block,Random.Seed)
randomlyChangeBlocksAround collision lis_ see_ level bal_ =

                                                  case head lis_ of
                                                         Nothing->
                                                             (collision::[],see_)
                                                         Just _->

                                                             let lis_unchanged =
                                                                     filter (\{i,j,sta,x,y,typ}->(i< -1+collision.i || i> 1+collision.i || j< -1+collision.j || j> 1+collision.j  || typ<3 || sta==Fade || sta==Vanished )) lis_
                                                             in


                                                             let lis_change =
                                                                     filter (\{i,j,sta,x,y,typ}->(i>= -1+collision.i && i<= 1+collision.i && j>= -1+collision.j && j<= 1+collision.j  && typ>=3 && sta/=Fade && sta/=Vanished )) lis_
                                                             in


                                                             case head lis_change of
                                                                 Nothing->
                                                                     (collision::lis_ ,see_ )
                                                                 Just first->
                                                                     let
                                                                              (num,newSeed) =
                                                                                                Random.step (Random.int 3 14 ) see_
                                                                     in


                                                                     let num_=
                                                                             if num<9 then
                                                                                 3+modBy 3 num
                                                                             else if (num>=9&&num<12&&level==1) then
                                                                                 num - 6
                                                                             else
                                                                                 num
                                                                     in


                                                                     let color =
                                                                             Maybe.withDefault (rgb 104 35 182)  (List.head(List.drop num_ col_bank))
                                                                     in


                                                                     let newBlock=
                                                                             {first|typ=num_,col=color,sta=Normal}
                                                                     in


                                                                     let subFunction=
                                                                             randomlyChangeBlocksAround collision (drop 1 lis_change) newSeed level bal_
                                                                     in


                                                                     (append lis_unchanged (newBlock::(Tuple.first subFunction)), (Tuple.second subFunction))


setCollisionBlockOnFire : Block->List Block->List Block
setCollisionBlockOnFire collision lis_ =
                                         let lis_unchanged =
                                                 filter (\{i,j,typ,sta}->(i< -1+collision.i || i> 1+collision.i || j< -1+collision.j || j> 1+collision.j || sta==Darken || (typ>=12 && typ<15) )) lis_
                                         in


                                         let lis_change =
                                                 filter (\{i,j,typ,sta}->(i>= -1+collision.i && i<= 1+collision.i && j>= -1+collision.j && j<= 1+collision.j && sta/=Darken && (typ<12||typ>=15) )) lis_
                                         in


                                         let lis_darken =
                                                 filter (\{typ}->( typ>=0 && typ<3 )) lis_change
                                         in


                                         let lis_fade=
                                                 filter (\{typ,sta}->( (typ<0 || typ>=3) &&  sta/= Vanished )) lis_change
                                         in


                                         let lis_vanished=
                                                 filter (\{typ,sta}->( (typ<0 || typ>=3) &&  sta== Vanished )) lis_change
                                         in


                                         let lis_easy_part=
                                                 concat [ (List.map (\someBlock->{someBlock|sta=Fade}) lis_fade), lis_vanished, collision::lis_unchanged ]

                                         in



                                         case head lis_darken of
                                             Nothing ->
                                                    lis_easy_part
                                             Just collision_  ->
                                                    setCollisionBlockOnFire {collision_|sta=Darken} (append lis_easy_part (drop 1 lis_darken) )


changeBlockColorAuto : Block->Block
changeBlockColorAuto someBlock =
                                   let col_ =
                                           someBlock.col
                                   in


                                   let (red_,green_,blue_) =
                                           (col_.red,col_.green,col_.blue)
                                   in


                                   let (col__,sta_) =
                                           if someBlock.sta==Darken then
                                               ({red=180.0+0.983*(-180.0+red_),green=180.0+0.983*(-180.0+green_),blue=180.0+0.983*(-180.0+blue_)}, Darken)
                                           else if someBlock.sta==Normal then
                                               (Maybe.withDefault (rgb 104 35 182)  (List.head(List.drop someBlock.typ col_bank)), Normal)
                                           else if someBlock.sta==Fade then
                                               if (red_+green_+blue_)<700 then
                                                   ({red=min 255.0 1.012*red_,green=min 255.0 1.012*green_,blue=min 255.0 1.012*blue_}, Fade)
                                               else
                                                   (Maybe.withDefault (rgb 104 35 182)  (List.head(List.drop someBlock.typ col_bank)), Vanished)
                                           else
                                               (col_,Vanished)

                                   in
                                       {someBlock|col=col__,sta=sta_}


healThreeBlocks : List Block -> Int  -> Ball -> List Block
healThreeBlocks lis_ level bal_=
                      let lis_unchanged=
                            filter (  \someBlock -> ( someBlock.sta/=Vanished || someBlock.typ<3 || someBlock.typ>=12 || (judgeCollideBlock someBlock bal_ 0 )==True )) lis_
                      in


                      let lis_change=
                            filter (  \someBlock -> ( someBlock.sta==Vanished && someBlock.typ>=3 && someBlock.typ<12 && (judgeCollideBlock someBlock bal_ 0 )==False)) lis_
                      in


                      case (head lis_change, head (drop 1 lis_change), head (drop 2 lis_change)) of
                              (Just first, Just second, Just third)->
                                  {first|sta=Normal, typ=decideType first.typ level }::{second|sta=Normal, typ=decideType second.typ level }::{third|sta=Normal, typ=decideType third.typ level }::(append (drop 3 lis_change) lis_unchanged )
                              (Just first, Just second, Nothing)->
                                  {first|sta=Normal, typ=decideType first.typ level }::{second|sta=Normal, typ=decideType second.typ level }::lis_unchanged
                              (Just first, Nothing, Nothing)->
                                  {first|sta=Normal, typ=decideType first.typ level }::lis_unchanged
                              _->
                                  lis_unchanged


decideType : Int -> Int -> Int
decideType typ level =
   if (level/=1 && typ>=3 && typ<6 ) then
        typ + 6
    else
        typ


changeCollisionBlockStatus : Block->Block
changeCollisionBlockStatus someBlock =
                         case someBlock.sta of
                             Normal->
                                 {someBlock|sta=Vanished}
                             _->
                                 someBlock





