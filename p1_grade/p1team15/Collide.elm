module Collide exposing (getCrossPoint,getLineAngle,CrossPoint, cpinit, crosstostring, choosemin, collideborder, collidepaddle, collideShape, collideShapes)
import Shapes exposing(Shape,calculatePoint,GeneralID(..))
import CreateShape exposing (distanceCenter)
import String exposing (String)
type alias CrossPoint = 
    { 
        point :  (Float,Float)
    , distance : Float
    , theta :Float
    , id :GeneralID
    , ori:(Float,Float)
    ,next : (Float,Float)
    ,p1:(Float,Float)
    ,p2:(Float,Float)
    }
cpinit :CrossPoint
cpinit=
    {
        point = (0,0),
        distance = 0,
        theta=0,
        id= ShapeID 0,
        ori = (0,0),
        next= (0,0),
        p1= (0,0),
        p2= (0,0)
    }
crosstostring : CrossPoint ->String
crosstostring c =
    let
        temp = case c.id of
                ShapeID k ->
                    "Shape" ++ String.fromInt k
                BorderID k->
                    "Border" ++ String.fromInt k
                PaddleID k-> 
                    "Paddle" ++ String.fromInt k
                _->
                    ""
    in
        "p= "++ String.fromInt (floor (Tuple.first c.point)) ++ "," ++ String.fromInt (floor  (Tuple.second c.point) )++ "  d="++ String.fromInt (floor  c.distance) ++"  t="
        ++ String.fromInt (floor  (c.theta*10000)) ++ "  ID="++ temp ++ "   p1="++ String.fromInt (floor (Tuple.first c.p1)) ++ "," ++ String.fromInt (floor  (Tuple.second c.p1) )++
        "  p2= "++ String.fromInt (floor (Tuple.first c.p2)) ++ "," ++ String.fromInt (floor  (Tuple.second c.p2) )++
        "  ori= "++ String.fromInt (floor (Tuple.first c.ori)) ++ "," ++ String.fromInt (floor  (Tuple.second c.ori) )++
        "  next= "++ String.fromInt (floor (Tuple.first c.next)) ++ "," ++ String.fromInt (floor  (Tuple.second c.next) )

getLineAngle :  (Float,Float)->(Float,Float)->Float
getLineAngle (x1,y1) (x2,y2)=
    atan ((y2-y1)/(x2-x1))
getCrossPoint : (Float,Float)->(Float,Float)->(Float,Float)->(Float,Float)->GeneralID-> Maybe CrossPoint
getCrossPoint (x1,y1) (x2,y2) (x3,y3) (x4,y4) index=
    let
        topx=-(-x2*x3*y1 + x2*x4*y1 + x1*x3*y2 - x1*x4*y2 + x1*x4*y3 - x2*x4*y3 - x1*x3*y4 + x2*x3*y4)
        topy=-(x2*y1*y3 - x4*y1*y3 - x1*y2*y3 + x4*y2*y3 - x2*y1*y4 + x3*y1*y4 + x1*y2*y4 - x3*y2*y4)
        bottomxy= x3*y1 - x4*y1 - x3*y2 + x4*y2 - x1*y3 + x2*y3 + x1*y4 - x2*y4

        
    in
         if bottomxy==0 then
            Nothing
         else
            let
                cx= topx/bottomxy
                cy= -topy/bottomxy

                max1=max x1 x2
                min1 = min x1 x2
                max2 = max x3 x4
                min2 = min x3 x4

                max12=max y1 y2
                min12 = min y1 y2
                max22 = max y3 y4
                min22 = min y3 y4
                fs=(cx>min1 && cx <max1 && cy>min12 && cy <max12)||(min1==max1 && cy>min12 && cy <max12)||(cx>min1 && cx <max1 && min12==max12)
                ss=(cx>min2 && cx <max2 && cy>min22 && cy <max22)||(min2==max2 && cy>min22 && cy <max22)||(cx>min2 && cx <max2 && min22==max22)
            in
                 --if (cx>min1 && cx <max1 && cx>min2 && cx< max2)||(cy>min12 && cy <max12 && cy>min22 && cy<max22) then
                 if fs && ss then
                    Just {point=(cx,cy),distance= distanceCenter (cx,cy) (x3,y3),theta = getLineAngle (x1,y1) (x2,y2),id=index,ori=(x3,y3),next=(x4,y4),p1=(x1,y1),p2=(x2,y2)}
                 else
                    Nothing



choosemin : Maybe CrossPoint -> Maybe CrossPoint -> Maybe CrossPoint
choosemin x a=
     if x==Nothing then
        a
     else
         if a==Nothing then
            x
         else
            let
                dist1=(Maybe.withDefault cpinit x).distance
                dist2=(Maybe.withDefault cpinit a).distance
            in
                 if dist1<dist2 then
                    x
                 else
                    a


collideShape : (Float,Float)->(Float,Float) -> Float->Shape->Maybe CrossPoint
collideShape  (x1,y1) (x2,y2) r shape=
    let
        points = Shapes.calculateCollideBorder shape r
        leng=-1+ List.length points
        p1=List.take leng points
        p2=List.drop 1 points

        answerlist = List.map2 (\x y->getCrossPoint x y (x1,y1) (x2,y2) shape.id ) p1 p2
        finalanswer = List.foldl choosemin Nothing answerlist
    in
        finalanswer

collideShapes :List Shape ->(Float,Float)->(Float,Float)-> Float ->Maybe CrossPoint
collideShapes lishape ori next r =

    let
        maylist = List.filter (\x-> distanceCenter x.center ori < r+x.radius + distanceCenter ori next && Tuple.second x.center>x.radius ) lishape
        answerlist = List.map (collideShape ori next r) maylist
        finalanswer = List.foldl choosemin Nothing answerlist
    in
        finalanswer

collidepaddle : Shapes.Paddle -> (Float,Float)->(Float,Float)-> Float ->Maybe CrossPoint
collidepaddle pad ori next r=
    let
        shape = Shapes.paddletoShape pad
    in
        collideShape ori next r shape

collideborder : (Float,Float)->(Float,Float)->(Float,Float)-> Float -> Maybe CrossPoint
collideborder (wid,hei) ori next r=
    let
        front= [(0,r),(r,0),(wid-r,hei),(wid,hei-r)]
        back = [(wid,r),(r,hei),(wid-r,0),(0,hei-r)]
        id= [BorderID 1,BorderID 3,BorderID 4,BorderID 2 ]
        anslist= List.map3 (\x y z->getCrossPoint x y ori next z) front back id
        finalanswer = List.foldl choosemin Nothing anslist
    in
        finalanswer