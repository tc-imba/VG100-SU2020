module Geometry exposing (..)


type alias Point =
    ( Float, Float )


type alias Rectangle =
    { a : Point
    , b : Point
    , c : Point
    , d : Point
    }


type alias Circle =
    { pos : Point, r : Float }


type alias Line =
    ( Point, Point )


lineTranslate : Point -> Line -> Line
lineTranslate disp ( a, b ) =
    ( pointAdd disp a, pointAdd disp b )


norm : Point -> Float
norm ( x, y ) =
    sqrt (x ^ 2 + y ^ 2)


dotProduct : Point -> Point -> Float
dotProduct ( x1, y1 ) ( x2, y2 ) =
    x1 * x2 + y1 * y2


stretch : Float -> Point -> Point
stretch t ( x, y ) =
    ( t * x, t * y )


pointAdd : Point -> Point -> Point
pointAdd ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


pointDiff : Point -> Point -> Point
pointDiff ( x1, y1 ) ( x2, y2 ) =
    ( x2 - x1, y2 - y1 )


det : Float -> Float -> Float -> Float -> Float
det a11 a12 a21 a22 =
    a11 * a22 - a12 * a21


reflect : Point -> Point -> Point
reflect vector axis =
    let
        adir =
            stretch (1.0 / norm axis) axis

        projection =
            stretch (dotProduct vector adir) adir

    in
    pointDiff (stretch 2 projection) vector


pointInRectangle : Point -> Rectangle -> Bool
pointInRectangle p rec =
    let
        a =
            rec.a

        b =
            rec.b

        d =
            rec.d

        ap =
            pointDiff a p

        ab =
            pointDiff a b

        ad =
            pointDiff a d

        ap_ab =
            dotProduct ap ab

        ab_ab =
            dotProduct ab ab

        ap_ad =
            dotProduct ap ad

        ad_ad =
            dotProduct ad ad
    in
    ap_ab >= 0 && ab_ab >= ap_ab && ad_ad >= ap_ad && ap_ad >= 0


type Intersect
    = Intersect { quadrant : Int, lambda : Float, axis : Point }
    | NotIntersect




circleIntersectLine_ : Circle -> Line -> Bool
circleIntersectLine_ circ ( ( x1, y1 ), ( x2, y2 ) ) =
    let
        x =
            circ.pos |> Tuple.first

        y =
            circ.pos |> Tuple.second

        r =
            circ.r

        lineLengthSquared =
            (x2 - x1) ^ 2 + (y2 - y1) ^ 2

        a =
            lineLengthSquared

        b =
            2 * (x2 - x1) * (x1 - x) + 2 * (y2 - y1) * (y1 - y)

        c =
            (x1 - x) ^ 2 + (y1 - y) ^ 2 - r ^ 2

        delta =
            b ^ 2 - 4 * a * c

        lambda_1 =
            (negate b - sqrt delta) / (2 * a)

        lambda_2 =
            (negate b + sqrt delta) / (2 * a)
    in
    (delta /= 0) && (((0 < lambda_1) && (lambda_1 < 1)) || ((0 < lambda_2) && (lambda_2 < 1)))


circleIntersectLine : Circle -> Line -> Intersect
circleIntersectLine circ ( ( x1, y1 ), ( x2, y2 ) ) =
    let
        x =
            circ.pos |> Tuple.first

        y =
            circ.pos |> Tuple.second

        r =
            circ.r

        lineLengthSquared =
            (x2 - x1) ^ 2 + (y2 - y1) ^ 2

        a =
            lineLengthSquared

        b =
            2 * (x2 - x1) * (x1 - x) + 2 * (y2 - y1) * (y1 - y)

        c =
            (x1 - x) ^ 2 + (y1 - y) ^ 2 - r ^ 2

        delta =
            b ^ 2 - 4 * a * c

        lambda_1 =
            (negate b - sqrt delta) / (2 * a)

        lambda_2 =
            (negate b + sqrt delta) / (2 * a)

        nx_1 =
            (1 - lambda_1) * x1 + lambda_1 * x2 - x

        ny_1 =
            (1 - lambda_1) * y1 + lambda_1 * y2 - y

        nx_2 =
            (1 - lambda_2) * x1 + lambda_2 * x2 - x

        ny_2 =
            (1 - lambda_2) * y1 + lambda_2 * y2 - y
    in
    if delta /= 0 then
        if 0 < lambda_1 && lambda_1 < 1 then
            if nx_1 > 0 && ny_1 > 0 then
                Intersect { quadrant = 1, lambda = lambda_1, axis = ( nx_1, ny_1 ) }

            else if nx_1 < 0 && ny_1 > 0 then
                Intersect { quadrant = 2, lambda = lambda_1, axis = ( nx_1, ny_1 ) }

            else if nx_1 < 0 && ny_1 < 0 then
                Intersect { quadrant = 3, lambda = lambda_1, axis = ( nx_1, ny_1 ) }

            else if nx_1 > 0 && ny_1 < 0 then
                Intersect { quadrant = 4, lambda = lambda_1, axis = ( nx_1, ny_1 ) }

            else
                NotIntersect

        else if 0 < lambda_2 && lambda_2 < 1 then
            if nx_2 > 0 && ny_2 > 0 then
                Intersect { quadrant = 1, lambda = lambda_2, axis = ( nx_2, ny_2 ) }

            else if nx_2 < 0 && ny_2 > 0 then
                Intersect { quadrant = 2, lambda = lambda_2, axis = ( nx_2, ny_2 ) }

            else if nx_2 < 0 && ny_2 < 0 then
                Intersect { quadrant = 3, lambda = lambda_2, axis = ( nx_2, ny_2 ) }

            else if nx_2 > 0 && ny_2 < 0 then
                Intersect { quadrant = 4, lambda = lambda_2, axis = ( nx_2, ny_2 ) }

            else
                NotIntersect

        else
            NotIntersect

    else
        NotIntersect


lineIntersectLine : Line -> Line -> Intersect
lineIntersectLine ( ( x1, y1 ), ( x2, y2 ) ) ( ( x3, y3 ), ( x4, y4 ) ) =
    let
        -- let ri = (xi, yi), lij = (ri, rj)
        -- parametric equation of l12 : lambda r2 + ( 1 - lambda ) r1
        -- parametric equation of l34 : mu r4 + (1 - mu ) r3
        detcoeff =
            det (x2 - x1) (x4 - x3) (y2 - y1) (y4 - y3)

        detlambda =
            det (x4 - x1) (x4 - x3) (y4 - y1) (y4 - y3)

        detmu =
            det (x2 - x1) (x4 - x1) (y2 - y1) (y4 - y1)

        lambda =
            detlambda / detcoeff

        mu =
            detmu / detcoeff

        -- the normal vector of l34
        normal =
            ( y3 - y4, x4 - x3 )
    in
    if 0 <= lambda && lambda <= 1 && 0 <= mu && mu <= 1 then
        Intersect { quadrant = 0, lambda = lambda, axis = normal }

    else
        NotIntersect


circleIntersectRectangle_ : Circle -> Rectangle -> Bool
circleIntersectRectangle_ circ rect =
    let
        p =
            circ.pos

        a =
            rect.a

        b =
            rect.b

        c =
            rect.c

        d =
            rect.d
    in
    pointInRectangle p rect
        || circleIntersectLine_ circ ( a, b )
        || circleIntersectLine_ circ ( b, c )
        || circleIntersectLine_ circ ( c, d )
        || circleIntersectLine_ circ ( d, a )


toLambda : Intersect -> Float
toLambda i =
    case i of
        NotIntersect ->
            2

        -- if intersection exist, it should not be larger than 1
        -- therefore 2 (>1) indicates no intersection
        Intersect r ->
            r.lambda


toNormal : Intersect -> ( Float, Float )
toNormal i =
    case i of
        NotIntersect ->
            ( 0, 0 )

        Intersect r ->
            r.axis



-- sortBy : (a -> comparable) -> List a -> List a
-- the intersection that happens next should, if it exists, have the least lambda


findPriorIntersection : List Intersect -> Intersect
findPriorIntersection li =
    List.sortBy toLambda li
        |> List.head
        |> Maybe.withDefault NotIntersect


filterByQuarant : Int -> Intersect -> Intersect
filterByQuarant n i =
    case i of
        NotIntersect ->
            NotIntersect

        Intersect r ->
            if r.quadrant == n then
                i

            else
                NotIntersect


circleIntersectRectangle : Circle -> Point -> Rectangle -> Intersect
circleIntersectRectangle circ_ displacement rec =
    let
        r =
            circ_.r

        circ =
            { circ_ | pos = pointAdd circ_.pos displacement }

        line =
            ( circ_.pos, circ.pos )

        a =
            rec.a

        b =
            rec.b

        c =
            rec.c

        d =
            rec.d

        c1 =
            Circle c r

        -- collision happen in quadrant II
        c3 =
            Circle b r

        -- collision happen in quadrant III
        c5 =
            Circle a r

        -- collision happen in quadrant IV
        c7 =
            Circle d r

        l2 =
            lineTranslate ( 0, r ) ( b, c )

        l4 =
            lineTranslate ( negate r, 0 ) ( a, b )

        l6 =
            lineTranslate ( 0, negate r ) ( d, a )

        l8 =
            lineTranslate ( r, 0 ) ( c, d )

        inter1 =
            circleIntersectLine c1 line |> filterByQuarant 1

        inter3 =
            circleIntersectLine c3 line |> filterByQuarant 2

        inter5 =
            circleIntersectLine c5 line |> filterByQuarant 3

        inter7 =
            circleIntersectLine c7 line |> filterByQuarant 4

        inter2 =
            lineIntersectLine line l2

        inter4 =
            lineIntersectLine line l4

        inter6 =
            lineIntersectLine line l6

        inter8 =
            lineIntersectLine line l8

        intersectList =
            [ inter1, inter2, inter3, inter4, inter5, inter6, inter7, inter8 ]

        prior_ =
            findPriorIntersection intersectList

        prior =
            if toLambda prior_ <= 1 then
                prior_

            else
                NotIntersect
    in
    prior


cartesianProduct : List a -> List b -> List ( a, b )
cartesianProduct l1 l2 =
    List.foldr (\li1 -> \li2 -> li1 ++ li2)
        []
        (List.map (\x -> List.map (\y -> ( x, y )) l2) l1)


isInt : Float -> Bool
isInt x =
    if abs (x - toFloat (floor x)) < 0.000001 then
        True

    else
        False
