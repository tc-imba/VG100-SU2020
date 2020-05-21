module Main exposing (..)

import Playground exposing (..)


houseColor =
    rgb 16 179 201


leftWindowColor =
    rgb 12 174 3


rightWindowColor =
    rgb 182 67 39


roofColor =
    rgb 133 31 59


doorColor =
    rgb 244 222 33


main =
    game view update ( 0, 0, 0 )


house =
    group
        [ square houseColor 300
        , square leftWindowColor 50 |> move -75 75
        , square rightWindowColor 50 |> move 75 75
        , rectangle doorColor 100 150 |> moveY -75
        , polygon roofColor [ ( -150, 150 ), ( 150, 150 ), ( 0, 300 ) ]
        ]


view computer ( x, y, theta ) =
    [ house |> move x y |> rotate theta  ]


toTheta : Keyboard -> Number
toTheta keyboard =
    if keyboard.space then
        1

    else
        0


update computer ( x, y, theta ) =
    ( x + 2 * toX computer.keyboard
    , y + 2 * toY computer.keyboard
    , theta - toTheta computer.keyboard
    )
