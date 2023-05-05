module Hexagon exposing (..)

import Canvas


type alias GridPoint =
    ( Int, Int )


type alias Hexagon =
    { grid : GridPoint
    , center : Canvas.Point
    , corners : ( Canvas.Point, List Canvas.Point )
    }


hexUnitPoints : ( Canvas.Point, List Canvas.Point )
hexUnitPoints =
    let
        root3Over2 =
            3 ^ 0.5 / 2
    in
    ( ( 1, 0 ), [ ( 0.5, root3Over2 ), ( -0.5, root3Over2 ), ( -1, 0 ), ( -0.5, -root3Over2 ), ( 0.5, -root3Over2 ), ( 1, 0 ) ] )


hexHorizSpacing : Int -> Float
hexHorizSpacing hexSize =
    toFloat (3 * hexSize) / 2


hexVertSpacing : Int -> Float
hexVertSpacing hexSize =
    sqrt 3 * toFloat hexSize


hexagon : Int -> GridPoint -> Hexagon
hexagon hexSize gridPoint =
    let
        ( gx, gy ) =
            gridPoint |> Tuple.mapBoth toFloat toFloat

        hexSizeF =
            toFloat hexSize

        {- If the row (x value of grid point) is odd then push down the center by half a vertical spacing -}
        verticalShove =
            if modBy 2 (Tuple.first gridPoint) == 1 then
                hexVertSpacing hexSize / 2

            else
                0

        center =
            ( hexSizeF + gx * hexHorizSpacing hexSize, (1 / 2 + gy) * hexVertSpacing hexSize + verticalShove )

        ( x, y ) =
            center

        unitToCornerMapper =
            Tuple.mapBoth (\cosTheta -> x + hexSizeF * cosTheta) (\sinTheta -> y + hexSizeF * sinTheta)

        corners =
            Tuple.mapBoth unitToCornerMapper (List.map unitToCornerMapper) hexUnitPoints
    in
    { grid = gridPoint
    , center = center
    , corners = corners
    }
