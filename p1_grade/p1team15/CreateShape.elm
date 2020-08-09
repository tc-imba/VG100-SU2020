module CreateShape exposing ( validRandom, randomPosition, randomAllPosition, distanceCenter, recommandradius, createshape)
import Random
import Model exposing (Model)
import Shapes exposing (Shape,GeneralID(..))
import Global exposing (ProgramState(..),LevelInfo,ShopState,PadBallState,getpbState,getMoneyList,coinsConsumption)

type alias LeverInfo=
    {
        possList : List Float
        ,number : Int
        ,duration : Float

    }
{-
createacolor : Random.Seed -> Int -> List Shapes.Color -> (List Shapes.Color,Random.Seed)
createacolor seed number listorigin=
    let
        (color,nextseed) = Shapes.createRandomColor seed
    in

        if number==0 then
            (listorigin,seed)
        else
            createacolor nextseed (number-1) (List.append listorigin [color])
-}

getColor : Int -> Shapes.Color
getColor tttype =
    case tttype of
        1 -> { red=238, green=130, blue=238 }
        2 -> { red=34,  green=139, blue=34 }
        3 -> { red=178, green=34,  blue=34 }
        4 -> { red=65,  green=105, blue=255 }
        5 -> { red=218, green=165, blue=32 }
        6 -> { red=255, green=0,   blue=0 }
        7 -> { red=255, green=200, blue=0 }
        8 -> { red=0,   green=139, blue=139 }
        0 -> { red=75,  green=100, blue=13 }
        10 -> { red=255 , green=255, blue=255}
        _ -> { red=0, green=0 ,blue=0 }
{-
createAllColor : List (Int, Maybe (Float,Float)) -> Int -> List Shapes.Color -> List Shapes.Color
createAllColor typelist number previous=
    if number == 0 then
        previous
    else
        let
            tttype=Tuple.first (List.head typelist)
            remainlist=List.drop 1 typelist
        in
            createAllColor remainlist (number-1) (List.append previous [(getColor tttype)] )
-}

validRandom : Random.Seed-> Float ->Float-> Float-> List (Float,Float) -> (Float,Float,Random.Seed)
validRandom seed distance width height originlist=
    let
        (x,seed1)=Random.step (Random.int (floor (distance/2)) (floor (width-distance))) seed
        (y,seed2)=Random.step (Random.int (floor (distance/2)) (floor (height-distance))) seed1
        fx= toFloat x
        fy= toFloat y
        notvalidelement = List.filter (\element-> (distanceCenter (fx,fy) element) < distance*1.4) originlist
    in
         if List.isEmpty notvalidelement  then
            (fx,fy,seed2)
         else
            validRandom seed2 distance width height originlist

randomPosition : Float ->Float-> Float->Int->Random.Seed ->List (Float,Float) ->(List (Float,Float),Random.Seed)
randomPosition distance width height numberleft seed origionList=
    let
        (nextx,nexty,nextseed)= validRandom seed distance width height origionList
    in
         if numberleft==0 then
            (origionList,seed)
         else
            randomPosition distance width height (numberleft-1) nextseed  (List.append origionList [(nextx,nexty)])

randomAllPosition : Model ->Random.Seed -> Int ->(List (Float,Float),Random.Seed,Float)
randomAllPosition model seed number=
    let
        distance = 2* recommandradius model.size
        (w,h)= model.size
        widlength = w-distance 
        heilength =max (Tuple.second model.size* 0.5) ( pi * distance*distance /4 * (toFloat number) * 3 *1.96 / widlength) + distance
        dy=0.5*h-heilength
        (t1,t2)=randomPosition distance widlength heilength (number) seed []
    in
        (t1,t2, dy)

distanceCenter : (Float,Float)->(Float,Float)->Float
distanceCenter (x1,y1) (x2,y2) =
    sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))

recommandradius : (Float,Float) -> Float
recommandradius (width,height) = 
    let
        widrestrict = max (width/40) 50
        heirestrict = min (height/10) widrestrict
    in
    heirestrict/1.2

randomTypeHelper : Random.Seed ->Float -> List Float ->(Int, Maybe (Float,Float),Random.Seed)
randomTypeHelper s r possibilityList=
    let
        (x,newseed)= Random.step (Random.float 0 1) s
        tttype =10 - List.length (List.filter (\a -> a>x) possibilityList)
        ttype=
            if tttype == 9 then
                0
            else
                tttype
        size =  case tttype of
            1 ->
                Just (1.6*r,1.2*r)
            2 ->
                Just (4*r/sqrt 5,2*r/sqrt 5)
            _ ->
                Nothing
    in
        (ttype,size,newseed)
--decide one block's type

randomType : Random.Seed -> Float -> Int -> List Float -> List (Int, Maybe (Float,Float)) -> List Int -> (List (Int, Maybe (Float,Float)), List Int ,Random.Seed)
randomType seed radius number possibilityList originList originSpecial =
    let
        (shapetype, size, newseed ) = randomTypeHelper seed radius possibilityList
        specialType= case shapetype of
            6 -> 1
            8 -> 2
            4 -> 3
            5 -> 4
            0 -> 5
            _ -> 0

    in
         if number==0 then
            (originList,originSpecial,seed)
         else
            randomType newseed radius (number-1) possibilityList (List.append originList [(shapetype,size)]) (List.append originSpecial [specialType])

--randomAllType : Random.Seed -> Float -> Int -> List Float -> (List (Int, Maybe (Float,Float)) ,Random.Seed)
--randomAllType seed radius number possibilitylist =
--    randomType seed radius number possibilitylist []



createshape : Model -> (List Shape,Random.Seed)
createshape model =
    let
        levelinfo= Global.getLevelSetting model.levelnumber
        height= Tuple.second model.size 
        r=recommandradius model.size
        width = Tuple.first model.size
        index=List.range 1 levelinfo.number
        (posit,nextseed,dy) = randomAllPosition model model.seed levelinfo.number
        position = List.map (\(x,y)->(x,y+dy)) posit

        (typelist,speciallist,nextseed2) = randomType nextseed r levelinfo.number levelinfo.possList [] []

        --colorlist = createAllColor typelist levelinfo.number []
        --(speciallist,nextseed4) = Random.step (Random.list levelinfo.number (Random.int -30 5 ) ) nextseed3

        --finalspecial = List.map (\x->  max x 0) speciallist

        maxduration=levelinfo.duration*2
        (durationlist,nextseed5) = Random.step (Random.list levelinfo.number (Random.float 1 maxduration ) ) nextseed2
        finalduration = List.map2 (\x y-> if y>0 then 1 else x) durationlist speciallist
        spdu = List.map2 (\x y->(x,y)) speciallist finalduration
        (anglelist,nextseed6) = Random.step (Random.list levelinfo.number (Random.float 0 (2*pi) ) ) nextseed5
    in
        
        (List.map4 (\x y a b->
        {radius = r, center =x, shapetype = (Tuple.first y),color = getColor (Tuple.first y) ,
        duration = (Tuple.second a),special = (Tuple.first a),size = (Tuple.second y),angle = b,id=ShapeID 0,opacity=1})
        position typelist spdu anglelist
        |> List.map2 (\x y-> {y| id = ShapeID x }) index,nextseed6)
