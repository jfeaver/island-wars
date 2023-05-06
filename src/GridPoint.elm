module GridPoint exposing (..)

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


canvasDistance : Int -> Float -> Float
canvasDistance hexSize gridDistance =
    gridDistance * hexVertSpacing hexSize


toCanvas : Int -> GridPoint -> Canvas.Point
toCanvas hexSize ( gx, gy ) =
    let
        {- If the row (x value of grid point) is odd then push down the center by half a vertical spacing -}
        verticalShove =
            if modBy 2 gx == 1 then
                hexVertSpacing hexSize / 2

            else
                0
    in
    ( toFloat hexSize + toFloat gx * hexHorizSpacing hexSize, canvasDistance hexSize (1 / 2 + toFloat gy) + verticalShove )


hexagon : Int -> GridPoint -> Hexagon
hexagon hexSize gridPoint =
    let
        hexSizeF =
            toFloat hexSize

        center =
            toCanvas hexSize gridPoint

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
