module Block exposing (..)

import Geometry exposing (..)
import Parameters exposing (..)


type alias Block =
    { i : Int, j : Int, hp : Int }


type alias Water =
    { i : Int, j : Int }


indiceExist : ( Int, Int ) -> List Block -> Bool
indiceExist ( m, n ) lb =
    List.any (\x -> x.i == m && x.j == n) lb


absurdIndice =
    ( 1000, 1000 )


indicesIntersect : List ( Int, Int ) -> List Block -> Bool
indicesIntersect li lb =
    let
        head =
            List.head li |> Maybe.withDefault absurdIndice
    in
    if List.length li == 0 then
        False

    else if indiceExist head lb then
        True

    else
        indicesIntersect (List.drop 1 li) lb


selectByIndices : List ( Int, Int ) -> List Block -> List Block
selectByIndices li lb =
    List.filter (\b -> List.member ( b.i, b.j ) li) lb


initBlock : List Block
initBlock =
    map1_


toRectangle : Block -> Rectangle
toRectangle b =
    let
        x =
            toFloat b.i * gameParams.xStep

        y =
            toFloat b.j * gameParams.yStep

        rect =
            { a = ( x, y )
            , b = ( x, y + gameParams.xStep )
            , c = ( x + gameParams.xStep, y + gameParams.yStep )
            , d = ( x + gameParams.xStep, y )
            }
    in
    rect


hit : Int -> Block -> Block
hit atk block =
    let
        hp_ =
            block.hp

        hp =
            Basics.max 0 (hp_ - atk)
    in
    { block | hp = hp }


hitExplode : Block -> List ( Int, Int ) -> Block
hitExplode block li =
    let
        judge =
            List.filter
                (\x ->
                    abs (block.i - Tuple.first x) <= 2 && abs (block.j - Tuple.second x) <= 2
                )
                li
    in
    if List.isEmpty judge then
        block

    else
        { block | hp = 0 }


hitExp : Block -> List ( Int, Int ) -> ( Block, Int )
hitExp block li =
    let
        ( lst1, lst2 ) =
            List.partition (\x -> Tuple.first x == block.i && Tuple.second x == block.j) li
    in
    if List.isEmpty lst1 then
        ( block, 0 )

    else
        ( { block | hp = 0 }, 50 )


hitByIndices : Int -> List Block -> List ( Int, Int ) -> List ( Int, Int ) -> List ( Int, Int ) -> ( List Block, Int )
hitByIndices atk lb li blst elst =
    let
        judgeExplode =
            List.filter (\x -> List.member x blst) li

        judgeExp =
            List.filter (\x -> List.member x elst) li
    in
    if not (List.isEmpty judgeExp) then
        let
            ( lst1, lst2 ) =
                List.unzip (List.map (\x -> hitExp x judgeExp) lb)
        in
        ( lst1, List.sum lst2 )

    else if not (List.isEmpty judgeExplode) then
        ( List.map (\x -> hitExplode x judgeExplode) lb, 0 )

    else
        ( List.map
            (\x ->
                if List.member ( x.i, x.j ) li then
                    hit atk x

                else
                    x
            )
            lb
        , 0
        )


killBroken : List Block -> List Block
killBroken lb =
    List.filter (\{ hp } -> hp > 0) lb


blockArray : Int -> Int -> Int -> Int -> Int -> List Block
blockArray i1 j1 i2 j2 hp =
    cartesianProduct (List.range i1 i2) (List.range j1 j2)
        |> List.map (\( i, j ) -> Block i j hp)


map1_ =
    blockArray -5 3 7 14 1
        -- 1
        ++ blockArray -5 -5 4 2 1
        --2
        ++ blockArray 12 2 27 14 1
        --3
        ++ blockArray 14 -4 27 1 1
        --4
        ++ blockArray 17 -30 27 -5 1
        --5
        ++ blockArray 7 -30 12 -13 1
        --6
        ++ blockArray 10 -20 12 -16 1
        --9
        ++ blockArray 7 -12 9 -11 1
        --7
        ++ blockArray 8 -10 10 -10 1
        --8
        ++ blockArray -5 -30 6 -6 1
        --10
        ++ blockArray 7 -20 9 -16 1
        ++ blockArray 5 -3 6 -3 1
        ++ blockArray 6 7 8 7 1
        ++ blockArray 10 -1 13 -1 1
        ++ blockArray 10 -9 11 -8 1



--1. 点进去初始化 loadLevelInt, ClickGame Int (相应的view, update) 2.  reviseSpeed 3. targetPos 根据不同关不一样


map2_ =
    blockArray -5 7 4 14 2
        -- 1
        ++ blockArray 12 7 27 14 2
        --2
        ++ blockArray 17 6 27 6 2
        --3
        ++ blockArray 20 -8 27 5 2
        --4
        ++ blockArray 15 -11 27 -9 2
        --5
        ++ blockArray 12 -30 27 -12 2
        --6
        ++ blockArray -5 -15 7 -12 2
        ++ blockArray -5 -30 7 -16 2
        --7
        ++ blockArray -5 -11 1 -7 2
        --8
        ++ blockArray -5 -6 0 6 2
        --9
        ++ blockArray 10 6 12 6 2
        ++ blockArray 4 4 4 6 2
        ++ blockArray 7 1 7 1 2
        ++ blockArray 5 0 9 0 2
        ++ blockArray 6 -1 8 -1 2
        ++ blockArray 7 -2 7 -2 2
        ++ blockArray 12 0 14 0 2
        ++ blockArray 11 -2 12 -1 2
        ++ blockArray 14 -2 15 -1 2
        ++ blockArray 12 -3 14 -3 2
        ++ blockArray 13 -4 13 -4 2
        ++ blockArray 1 3 1 3 2
        ++ blockArray 2 1 2 3 2
        ++ blockArray 1 -6 2 -2 2
        ++ blockArray 3 -5 3 -5 2
        ++ blockArray 3 -6 3 -6 2
        ++ blockArray 15 4 16 5 2
        ++ blockArray 16 3 16 3 2
        ++ blockArray 18 0 18 3 2
        ++ blockArray 19 2 19 2 2
        ++ blockArray 14 -8 19 -7 2
        ++ blockArray 18 -6 19 -4 2
        ++ blockArray 8 -5 10 -4 2
        ++ blockArray 7 -5 7 -5 2
        ++ blockArray 9 -9 9 -6 2
        ++ blockArray 2 -11 4 -8 2
        ++ blockArray 5 -8 5 -8 2
        ++ blockArray 8 -12 8 -12 2
        ++ blockArray 2 -7 3 -7 2


map3_ =
    blockArray -5 3 6 14 4
        --1
        ++ blockArray 11 7 27 14 4
        --2
        ++ blockArray 13 3 27 6 4
        --3
        ++ blockArray 15 1 27 2 4
        --4
        ++ blockArray 17 -7 27 0 4
        --5
        ++ blockArray 16 -9 27 -8 4
        --6
        ++ blockArray 14 -12 27 -10 4
        --7
        ++ blockArray 16 -30 27 -13 2
        --8
        ++ blockArray -5 -30 12 -16 2
        --9
        ++ blockArray -5 -15 10 -14 4
        --10
        ++ blockArray -5 -13 9 -7 4
        --11
        ++ blockArray -5 -6 10 -5 4
        --12
        ++ blockArray -5 -4 11 -2 4
        --13
        ++ blockArray -5 -1 8 2 4
        --14
        ++ blockArray 9 9 10 9 4
        ++ blockArray 7 6 7 6 4
        ++ blockArray 11 3 12 3 4
        ++ blockArray 12 5 12 5 4
        ++ blockArray 12 -4 12 -4 4
        ++ blockArray 9 0 9 0 4
        ++ blockArray 10 -1 11 -1 4
        ++ blockArray 14 -1 15 -1 4
        ++ blockArray 15 0 15 0 4
        ++ blockArray 15 -8 15 -7 4
        ++ blockArray 15 -17 15 -17 4
        ++ blockArray 16 -7 16 -7 4
        ++ blockArray 13 -10 13 -10 4
        ++ blockArray 10 -10 10 -10 4
        ++ blockArray 16 -5 16 -5 4


explodeList1 =
    [ ( 11, -1 ), ( 11, -8 ), (17, -7) ]


explodeList2 =
    [ ( 7, 1 ), ( 13, 0 ) ]


explodeList3 =
    [ ( 10, -1 ), (9, -7 ), ( 14, -11 ) ]


expList1 =
    [ ( 6, -3 ), (14, -14), (4 , 2), (6, -3), (15, -2) ]


expList2 =
    [ ( 4, 4 ), ( 16, 3 ), ( 7, -2 ), ( 13, -3 ), ( 4, -8 ), ( 9, -9 ), ( 18, -3 ) ]


expList3 =
    [ ( 12, 3 ), ( 14, 0 ), ( 13, -10 ), ( 6, 0 ), ( 9, -11 ) ]


boomlst : Int -> List ( Int, Int )
boomlst n =
    if n == 1 then
        explodeList1

    else if n == 2 then
        explodeList2

    else
        explodeList3


explst : Int -> List ( Int, Int )
explst n =
    if n == 1 then
        expList1

    else if n == 2 then
        expList2

    else
        expList3
