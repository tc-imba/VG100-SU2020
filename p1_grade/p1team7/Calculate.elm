module Calculate exposing (..)


import Model exposing (Model,Keys)
import Object exposing (r)

ballLeftCoordinate : Model -> (Int, Int)
ballLeftCoordinate model =
    (floor ((model.ball_y - 8)/4), floor ((model.ball_x - 3)/4))

ballRightCoordinate : Model -> (Int, Int)
ballRightCoordinate model =
    (floor ((model.ball_y - 8)/4), floor ((model.ball_x - 1)/4))

ballUpCoordinate : Model -> (Int, Int)
ballUpCoordinate model =
    (floor ((model.ball_y - 9)/4), floor ((model.ball_x - 2)/4))

ballDownCoordinate : Model -> (Int, Int)
ballDownCoordinate model =
    (floor ((model.ball_y - 7)/4), floor ((model.ball_x - 2)/4))