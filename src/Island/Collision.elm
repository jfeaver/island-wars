module Island.Collision exposing (..)

import Canvas
import GridPoint
import Island exposing (Island)


insideMaxRadius : Int -> Canvas.Point -> Island -> Bool
insideMaxRadius hexSize ( x, y ) { center, size } =
    let
        ( xc, yc ) =
            GridPoint.toCanvas hexSize center

        maxRadius =
            Island.maxRadius size |> toFloat |> GridPoint.canvasDistance hexSize
    in
    ((xc - x) ^ 2 + (yc - y) ^ 2) ^ (1 / 2) <= maxRadius
