module Animation exposing (..)
import Messages exposing (Msg)
import Shapes exposing (GeneralID)
import Svg
import  Global
import  Svg.Attributes as Svga
type alias Transform =
    {
        scale : (Float,Float)
        ,opacity : Float
        ,cPosition:(Float,Float)
        ,rotationCenter: (Float,Float)
        ,rotate : Float
        ,size :(Float,Float)

    }
type alias AnimateInfo =
    {
        original: Transform
        ,final : Transform
        ,starttime : Float
        ,endtime: Float
        ,deletetime :Float
        ,full : Bool
    }
type alias ImageInfo =
    {
        animation : AnimateInfo
        ,path : String
        ,event :List (Svg.Attribute Msg)
        ,sceneID : Global.ProgramState
        ,imageID : ImageID
        ,zorder : Int
    }
type alias ShapeInfo =
    {
        animation : AnimateInfo
        ,shape : Shapes.Shape
        ,event :List (Svg.Attribute Msg)
        ,sceneID : Global.ProgramState
        ,shapeID : ShapeID
        ,zorder : Int
    }
type alias TextInfo =
    {
        animation : AnimateInfo
        ,text : String
        ,pt: Float
        ,refsize: (Float,Float)
        ,color: Shapes.Color
        ,sceneID : Global.ProgramState
        ,textID : TextID
        ,zorder : Int
    }
type TextID
    = ShopMoney
    | PowerMoney
    | LengthMoney
    | SpeedMoney
    | DurationMoney
    | ResultTexts
type ShapeID
    = SelectSingleDoubel Int
    | Level Int
    | LeveltoShop
    | BuyPower
    | BuyLength
    | StoreBack
    | BuySpeed
    | BuyDuration
    | LoseConfirm
    | LoseResume
    | LoseRetry
    | LoseMenu
    | LosetoShop
    | WinRetry
    | WinMenu
    | Next
    | LackMoneyOK
    | Gaming GeneralID
    | Dying

type ImageID
    = CoverBg
    | ModeSelectBg
    | LevelSelectBg
    | GameBackground
    | WinIcon
    | LoseBanner
    | WinResultBg
    | LoseResultBg
    | ShopBg
    | LackMoneyBanner
    | ShopBars
    | LevelIntro Int
    | Gamingiamge GeneralID

clearImageInfo:  Global.ProgramState -> Float ->List ImageInfo -> List ImageInfo
clearImageInfo  programState finishtime imageInfos =
        List.map (\x-> if x.sceneID /= programState then {x|animation = changedeletetime x.animation finishtime} else x) imageInfos

updateImageInfo :  Float ->List ImageInfo -> List ImageInfo
updateImageInfo now imageInfos  =
    List.filter (\x->x.animation.deletetime==0 || x.animation.deletetime>now) imageInfos

changedeletetime : AnimateInfo -> Float-> AnimateInfo
changedeletetime animateInfo t =
        {animateInfo| deletetime = t}


addImageInfo :  ImageInfo ->List ImageInfo ->List ImageInfo
addImageInfo next imageInfos  =
    if next.animation.full==False then imageInfos++ [next   ]
    else
        let
             newlist = List.map  (\x -> {x|animation=changedeletetime x.animation next.animation.endtime} )imageInfos
        in
            newlist++ [next]

clearShapeInfo:  Global.ProgramState -> Float ->List ShapeInfo -> List ShapeInfo
clearShapeInfo  programState finishtime shapeinfo =
        List.map (\x-> if x.sceneID /= programState then {x|animation = changedeletetime x.animation finishtime} else x) shapeinfo

updateShapeInfo :  Float ->List ShapeInfo -> List ShapeInfo
updateShapeInfo now shapeinfo  =
    List.filter (\x->x.animation.deletetime==0 || x.animation.deletetime>now) shapeinfo

replaceimage: List ImageInfo -> ImageInfo-> List ImageInfo
replaceimage imageInfos imageInfo =
    (List.filter (\x -> x.imageID /= imageInfo.imageID ) imageInfos )++[imageInfo]
replaceimages: List ImageInfo -> List ImageInfo-> List ImageInfo
replaceimages imageInfos new =
    List.foldl (\x a-> replaceimage a x) imageInfos new

replacetext: List TextInfo -> TextInfo-> List TextInfo
replacetext textInfos textInfo =
    (List.filter (\x -> x.textID /= textInfo.textID ) textInfos)++[textInfo]

replacetexts : List TextInfo ->List TextInfo-> List TextInfo
replacetexts textInfos new =
    List.foldl (\x a-> replacetext a x) textInfos new

replaceshape : List ShapeInfo -> ShapeInfo -> List ShapeInfo
replaceshape shapeInfos shapeInfo =
    (List.filter (\x -> x.shapeID /= shapeInfo.shapeID ) shapeInfos)++[shapeInfo]
replaceshapes : List ShapeInfo ->List ShapeInfo -> List ShapeInfo
replaceshapes shapeInfos new  =
    List.foldl (\x a-> replaceshape a x) shapeInfos new


--AnimateInfo: Transform->Transform->String->Float->Float->Float->Bool->Bool->AnimateInfo
--AnimateInfo Transform1 Transform2 path starttime endtime deletetime finished full =
--    {
--            original=Transform1
--            ,final =Transform2
--            ,path =path
--            ,starttime =starttime
--            ,endtime=endtime
--            ,deletetime =deletetime
--            ,finished =finished
--            ,full =full
--    }

imageInfoToSvg :Float-> ImageInfo -> Svg.Svg Msg
imageInfoToSvg now ani =
    let

        nowtrans= getMidAnimateInfo now ani.animation
        newrotatecenter=tupleHalf nowtrans.size
        ltp= ctolt nowtrans.cPosition nowtrans.size nowtrans.scale
        transstring = "translate("++ (String.fromFloat (Tuple.first ltp)) ++ ","++(String.fromFloat (Tuple.second ltp)) ++") "
                        ++ "scale("++ (String.fromFloat (Tuple.first nowtrans.scale)) ++ ","++(String.fromFloat (Tuple.second nowtrans.scale)) ++") rotate("
                        ++(String.fromFloat nowtrans.rotate)++ ","++ (String.fromFloat (Tuple.first newrotatecenter))++ ","++(String.fromFloat (Tuple.second newrotatecenter))++")"

    in
    Svg.image ([
        Svga.opacity (String.fromFloat nowtrans.opacity)
        ,Svga.width (String.fromFloat (Tuple.first nowtrans.size))
        ,Svga.height (String.fromFloat (Tuple.second nowtrans.size))
        ,Svga.xlinkHref ani.path
        ,Svga.transform transstring
    ]++ani.event)[]

textInfoToSvg :(Float,Float)->Float-> TextInfo -> Svg.Svg Msg
textInfoToSvg modelsize now ani =
    let

        nowtrans= getMidAnimateInfo now ani.animation
        newrotatecenter=(0,0)
        actscale= tupleScale nowtrans.scale ((Tuple.first modelsize)/ (Tuple.first ani.refsize),(Tuple.second modelsize)/ (Tuple.second ani.refsize))
        ltp=  nowtrans.cPosition
        transstring = "translate("++ (String.fromFloat (Tuple.first ltp)) ++ ","++(String.fromFloat (Tuple.second ltp)) ++") "
                        ++ "scale("++ (String.fromFloat (Tuple.first actscale)) ++ ","++(String.fromFloat (Tuple.second actscale)) ++") rotate("
                        ++(String.fromFloat nowtrans.rotate)++ ","++ (String.fromFloat (Tuple.first newrotatecenter))++ ","++(String.fromFloat (Tuple.second newrotatecenter))++")"

    in
    Svg.text_ ([
        Svga.opacity (String.fromFloat nowtrans.opacity),
        Svga.fontWeight "bold",
        Svga.fill  (Shapes.toString ani.color),
        Svga.textAnchor "middle",
        Svga.fontSize (String.fromFloat ani.pt),
        Svga.alignmentBaseline "middle"
        ,Svga.transform transstring
    ])[Svg.text ani.text]

shapeInfoToSvg: Float-> ShapeInfo -> Svg.Svg Msg
shapeInfoToSvg now shapeInfo =
        let
            nowtrans= getMidAnimateInfo now shapeInfo.animation
            newrotatecenter= (0,0)
            ltp=nowtrans.cPosition
            transstring = "translate("++ (String.fromFloat (Tuple.first ltp)) ++ ","++(String.fromFloat (Tuple.second ltp)) ++") "
                            ++ "scale("++ (String.fromFloat (Tuple.first nowtrans.scale)) ++ ","++(String.fromFloat (Tuple.second nowtrans.scale)) ++") rotate("
                            ++(String.fromFloat nowtrans.rotate)++ ","++ (String.fromFloat (Tuple.first newrotatecenter))++ ","++(String.fromFloat (Tuple.second newrotatecenter))++")"
            shape=shapeInfo.shape
            points= if shape.shapetype /=0 then Shapes.calculatePoint shapeInfo.shape
                                            |>List.map (\x -> ((Tuple.first x) - (Tuple.first shape.center),(Tuple.second x) - (Tuple.second shape.center)) )
                    else
                        []
        in
        if shape.shapetype /=0 then
            Svg.polygon ([
                Svga.points (Shapes.pointstoString points)
                ,Svga.fill (Shapes.toString shape.color)
                ,Svga.fillOpacity (String.fromFloat shape.opacity)
                ,Svga.opacity (String.fromFloat nowtrans.opacity)
                ,Svga.transform transstring
            ]++shapeInfo.event)[]
        else
            Svg.circle
                        ([
                            Svga.cx  "0"
                            ,Svga.cy  "0"
                            ,Svga.fill (Shapes.toString shape.color)
                            ,Svga.r  (String.fromFloat shape.radius)
                            ,Svga.fillOpacity (String.fromFloat shape.opacity)
                            ,Svga.opacity (String.fromFloat nowtrans.opacity)
                            ,Svga.transform transstring
                        ]++ shapeInfo.event )[]


getactsize :(Float,Float)->(Float,Float)->(Float,Float  )
getactsize (scx, scy) (sx, sy) =
        (scx*sx,scy*sy)

tupleHalf : (Float,Float)->(Float,Float)
tupleHalf (x, y) = (x/2,y/2)

tupleScale :(Float,Float)->(Float,Float)->(Float,Float)
tupleScale (x, y) (sx, sy) =
    (x*sx,y*sy)

tupleAntiScale :(Float,Float)->(Float,Float)->(Float,Float)
tupleAntiScale (x, y) (sx, sy) =
    (x/sx,y/sy)

tupleAdd : (Float,Float)->(Float,Float)->(Float,Float)
tupleAdd (x, y) (x1, y1) =
    (x+x1,y+y1)

transforminit: (Float,Float)->(Float,Float) ->Transform
transforminit (x,y) (width,height) =
    {
        scale=(1,1)
        ,opacity = 1
        ,cPosition =(x,y)
        ,rotationCenter = (0,0)
        ,rotate=0
        ,size = (width,height)
    }

getMidValue : Float->Float->Float->Float->Float->Float
getMidValue start end now startvalue endvalue =
    let
        percentage= if (end - start)/=0 then (now - start) / (end - start) else 0

    in
        startvalue + percentage * (endvalue - startvalue)
getMidTuple : Float->Float->Float->(Float,Float)->(Float,Float)->(Float,Float  )
getMidTuple start end now (startvaluex,startvaluey) (endvaluex,endvaluey )=
    (getMidValue start end now startvaluex endvaluex,getMidValue start end now startvaluey endvaluey)

lttoc : (Float,Float)->(Float,Float)->(Float,Float)->(Float,Float)
lttoc (x1, y1) (sx, sy) (scx,scy) =
        (x1+sx*scx/2,y1+sy*scy/2)
ctolt : (Float,Float)->(Float,Float)->(Float,Float)->(Float,Float)
ctolt (x1, y1) (sx, sy) (scx,scy)=
        (x1 - sx*scx/2,y1 - sy*scy/2)

getMidTransform : Float->Float->Float->Transform->Transform->Transform
getMidTransform start end now ori next =
    let
        ft= getMidTuple start end now
        ff= getMidValue start end now
        --cp1=(((Tuple.first ori.ltPosition)+(Tuple.first ori.size)/2),((Tuple.second ori.ltPosition)+(Tuple.second ori.size)/2))
        --cp2=(((Tuple.first next.ltPosition)+(Tuple.first next.size)/2),((Tuple.second next.ltPosition)+(Tuple.second next.size)/2))
        --cpx= ft cp1 cp2
        --(scx,scy)=ft ori.scale next.scale
        --(sx,sy)=ft ori.size next.size
        --lp=((Tuple.first cpx) - sx/2*scx,(Tuple.second cpx) - sy/2*scy)
    in
    {
                scale = ft ori.scale next.scale
                ,opacity =ff ori.opacity next.opacity
                ,cPosition =ft ori.cPosition next.cPosition
                ,rotationCenter= ori.rotationCenter
                ,rotate = ff ori.rotate next.rotate
                ,size =ft ori.size next.size
    }



getMidAnimateInfo : Float->AnimateInfo-> Transform
getMidAnimateInfo now k =
   let
       ori=k.original
       next = k.final
       start= k.starttime
       end=k.endtime
   in
        if now<=end then getMidTransform start end now ori next else next