module Shape exposing (..)
import Html
import Svg exposing (..)
import List exposing (..)
import Game exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Svg
import Svg.Attributes as SvgAttrs




type Shape =
    Shape
    Float -- x
    Float -- y
    Int   -- angle
    Kind


type Kind
    = Circle Color Float
    | Rectangle Color Float Float
    | SpecialPolygon Color Int Float
    | Words Color String
    | Image Float Int


svgShape : Shape -> Svg msg
svgShape (Shape x y angle kind) =
    case kind of
        Circle color radius ->
            svgCircle color radius x y angle

        Rectangle color width height ->
            svgRectangle color width height x y angle

        SpecialPolygon color n radius ->
            svgSpecialPolygon color n radius x y angle

        Words color string ->
            svgWords color string x y angle

        Image rad typ ->
            svgImage rad x y angle typ


shapeAll : GameModel -> List Shape
shapeAll model =
    List.concat [(List.map shapeCircle model.bal), (List.map imageBall model.bal), [(imageBoard model)], (shapeList model), (imageList model)]


shapeCircle : Ball -> Shape
shapeCircle ball =
    Shape ball.x ball.y 0  (Circle ball.col ball.rad)

imageBall : Ball -> Shape
imageBall model =
    Shape model.x model.y 0 (Image model.rad 24)

shapeBoard : GameModel -> Shape
shapeBoard model =
    Shape model.bod.x model.bod.y 0 (Rectangle model.bod.col model.bod.wid model.bod.hei)

imageBoard : GameModel -> Shape
imageBoard model =
    Shape model.bod.x model.bod.y 0 (Image model.bod.wid 30)

shapeList : GameModel -> List Shape
shapeList model =
    List.map mapToShape (List.filter (\{sta} -> sta /= Vanished ) model.lis)


imageList : GameModel -> List Shape
imageList model =
    List.map mapToImage (List.filter (\{sta} -> sta /= Vanished ) model.lis)


mapToImage : Game.Block -> Shape
mapToImage unit=
    case unit.sha of
        3->
            Shape unit.x unit.y unit.ang (Image (0.8*unit.rad) unit.typ)
        _->
            Shape unit.x unit.y unit.ang (Image unit.rad unit.typ)


mapToShape : Game.Block ->Shape
mapToShape unit =
    Shape unit.x unit.y unit.ang (SpecialPolygon unit.col unit.sha (0.97 * unit.rad))


svgWords : Color -> String -> Float -> Float -> Int-> Svg msg
svgWords col str x y ang  =
    text_
    [  SvgAttrs.textAnchor "middle"
     , SvgAttrs.dominantBaseline "central"
     , SvgAttrs.fill ("rgb(" ++ String.fromFloat col.red ++ "," ++ String.fromFloat col.green ++ "," ++ String.fromFloat col.blue ++ ")")
     , SvgAttrs.transform ("translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat -y ++ ") rotate(" ++ String.fromInt -ang ++ ")")
    ]
    [  Html.text str
    ]


svgCircle : Color -> Float -> Float -> Float -> Int -> Svg msg
svgCircle col rad x y ang =
      Svg.circle
      [  SvgAttrs.r (String.fromFloat rad)
       , SvgAttrs.fill ("rgb(" ++ String.fromFloat col.red ++ "," ++ String.fromFloat col.green ++ "," ++ String.fromFloat col.blue ++ ")")
       , SvgAttrs.transform ("translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat -y ++ ") rotate(" ++ String.fromInt -ang ++ ")")
      ]
      []


svgRectangle : Color -> Float -> Float -> Float -> Float -> Int -> Svg msg
svgRectangle col wid hei x y ang  =
    rect
    [  width (String.fromFloat wid)
     , height (String.fromFloat hei)
     , fill ("rgb(" ++ String.fromFloat col.red ++ "," ++ String.fromFloat col.green ++ "," ++ String.fromFloat col.blue ++ ")")
     , transform ("translate(" ++ String.fromFloat (-wid/2+x) ++ "," ++ String.fromFloat (-hei/2+ -y) ++ ") rotate(" ++ String.fromInt -ang ++ ")")
    ]
    []


svgSpecialPolygon : Color -> Int -> Float -> Float -> Float -> Int -> Svg msg
svgSpecialPolygon col n rad x y ang =
    Svg.polygon
    [  points (stringSpecialPolygonPoints 0 n rad "")
     , fill ("rgb(" ++ String.fromFloat col.red ++ "," ++ String.fromFloat col.green ++ "," ++ String.fromFloat col.blue ++ ")")
     , transform ("translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat -y ++ ") rotate(" ++ String.fromInt -ang ++ ")")
    ]
    [
    ]

svgImage : Float -> Float -> Float -> Int -> Int -> Svg msg
svgImage rad x y ang typ =
    case typ //3 of
      0->
          Svg.image [  SvgAttrs.x ((String.fromFloat (x - rad/2)))
                     , SvgAttrs.y ("-" ++ (String.fromFloat (y + rad/2)))
                     , SvgAttrs.height (String.fromFloat (rad))
                     , SvgAttrs.width (String.fromFloat (rad))
                     , xlinkHref "picture/bomb.png"
                    ]
                    []
      2->
          Svg.image [   SvgAttrs.x ((String.fromFloat (x - rad/2)))
                      , SvgAttrs.y ("-" ++ (String.fromFloat (y + rad/2)))
                      , SvgAttrs.height (String.fromFloat (rad))
                      , SvgAttrs.width (String.fromFloat (rad))
                      , xlinkHref "picture/change.png"
                    ]
                    []

      3->
          Svg.image [   SvgAttrs.x ((String.fromFloat (x - rad/2)))
                    , SvgAttrs.y ("-" ++ (String.fromFloat (y + rad/2)))
                    , SvgAttrs.height (String.fromFloat (rad))
                    , SvgAttrs.width (String.fromFloat (rad))
                    , xlinkHref "picture/dsoldier.png"
                  ]
                  []

      4->
          Svg.image [   SvgAttrs.x ((String.fromFloat (x - rad/2)))
                      , SvgAttrs.y ("-" ++ (String.fromFloat (y + rad/2)))
                      , SvgAttrs.height (String.fromFloat (rad))
                      , SvgAttrs.width (String.fromFloat (rad))
                      , xlinkHref "picture/heal.png"
                    ]
                    []

      8 ->
          let
              by =
                  if y >= 0 then
                    "-" ++  (String.fromFloat (y + (rad+5)/2))
                  else
                    String.fromFloat -(y + (rad+5)/2)
          in

          Svg.image [ SvgAttrs.x ((String.fromFloat (x - (rad+5)/2)))
                    , SvgAttrs.y by
                    , SvgAttrs.height (String.fromFloat (rad+5))
                    , SvgAttrs.width (String.fromFloat (rad+5))
                    , SvgAttrs.rotate (String.fromInt -ang)
                    , xlinkHref "picture/sword.png"
                  ]
                  []

      10 ->
          Svg.image [   SvgAttrs.x ((String.fromFloat (x - rad*1.5)))
                      , SvgAttrs.y ((String.fromFloat -(y+5)))
                      , SvgAttrs.height (String.fromFloat (rad/6))
                      , SvgAttrs.width (String.fromFloat (3*rad))
                      , xlinkHref "picture/board.png"
                    ]
                    []

      1->
          Svg.image [   SvgAttrs.x ((String.fromFloat (x - rad/2)))
                      , SvgAttrs.y ("-" ++ (String.fromFloat (y + rad/2)))
                      , SvgAttrs.height (String.fromFloat (rad))
                      , SvgAttrs.width (String.fromFloat (rad))
                      , xlinkHref "picture/soldier.png"
                    ]
                    []
      _->
          Svg.image [   SvgAttrs.x ((String.fromFloat (x - rad/2)))
                      , SvgAttrs.y ("-" ++ (String.fromFloat (y + rad/2)))
                      , SvgAttrs.height (String.fromFloat (rad))
                      , SvgAttrs.width (String.fromFloat (rad))
                      , xlinkHref "picture/dsoldier.png"
                    ]
                    []


stringSpecialPolygonPoints : Int -> Int -> Float -> String -> String
stringSpecialPolygonPoints i n rad str =
    if i == n then
        str
    else
        let
            ang = 2 * pi * toFloat i / toFloat n
            x = rad * cos ang
            y = rad * sin ang
        in
    stringSpecialPolygonPoints (i + 1) n rad (str ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ " ")









