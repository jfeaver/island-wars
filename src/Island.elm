module Island exposing (..)

-- import GridPoint exposing (GridPoint)

import Island.Elevation exposing (Elevation2D)
import Island.IslandType exposing (IslandType(..))
import Point exposing (Point)
import Random
import Simplex


type alias WorldPoint =
    Point


type alias Island =
    { center : WorldPoint
    , size : Int
    , iType : IslandType
    }


type alias IslandOptions =
    { center : WorldPoint
    , size : Int
    , iType : IslandType
    }


init : IslandOptions -> Island
init { center, size, iType } =
    { center = center
    , size = size
    , iType = iType
    }


elevationFromNoise : Int -> Simplex.PermutationTable -> Elevation2D
elevationFromNoise _ permutationTable =
    let
        maxRad =
            -- maxRadius size
            11

        {- currently produces numbers between 1 and 17 -}
        elevationAt x y =
            let
                ( xf, yf ) =
                    ( toFloat x, toFloat y )

                distance =
                    (xf ^ 2 + yf ^ 2) ^ (1 / 2)

                maxRadiusF =
                    toFloat maxRad

                noiseConfig =
                    { steps = 3
                    , stepSize = 2.0
                    , persistence = 2.0
                    , scale = 3.0
                    }

                -- number between 0 and 2 scaled down further away from the center
                islandNoise =
                    (Simplex.fractal2d noiseConfig permutationTable xf yf + 1)
                        |> (*) (1 - ((distance / maxRadiusF) ^ 3))

                -- Noise values below this won't be used (elevation will be zero)
                noiseThreshold =
                    0.6

                -- How many possible elevation values are there?
                maxElevation =
                    22
            in
            if distance > maxRadiusF || islandNoise < noiseThreshold then
                0

            else
                (maxElevation * (islandNoise - noiseThreshold) / (2 - noiseThreshold))
                    |> ceiling

        radiusList =
            List.range -(maxRad - 1) (maxRad - 1)
    in
    radiusList
        |> List.map
            (\x ->
                radiusList
                    |> List.map (\y -> elevationAt x y)
            )


{-| Fold elevation map into a list of renderables. Use Canvas.group to combine into one renderable.
-}



-- renderable : Int -> Island -> Canvas.Renderable
-- renderable hexSize island =
--     let
--         hexRenderable : GridPoint -> Elevation -> Maybe Canvas.Renderable
--         hexRenderable gridPoint elevation =
--             let
--                 hex =
--                     GridPoint.hexagon hexSize gridPoint
--                 hexPath =
--                     Tuple.second hex.corners |> List.map lineTo
--                 color =
--                     Island.Elevation.color island.iType elevation
--             in
--             if elevation == 0 then
--                 Nothing
--             else
--                 Just <| shapes [ stroke color, fill color ] [ path (Tuple.first hex.corners) hexPath ]
--         gridPointFromIndices : Int -> Int -> GridPoint
--         gridPointFromIndices i j =
--             let
--                 ( xi, yi ) =
--                     island.center
--                 offset =
--                     island.size // 10
--             in
--             ( xi + i - offset, yi + j - offset )
--         columnFolder : Int -> Int -> Elevation -> List Canvas.Renderable -> List Canvas.Renderable
--         columnFolder i j elevation renderables =
--             case hexRenderable (gridPointFromIndices i j) elevation of
--                 Just hex ->
--                     hex :: renderables
--                 Nothing ->
--                     renderables
--         mapFolder : Int -> List Elevation -> List Canvas.Renderable -> List Canvas.Renderable
--         mapFolder i columnElevation renderables =
--             List.Extra.indexedFoldl (columnFolder i) renderables columnElevation
--     in
--     List.Extra.indexedFoldl mapFolder [] island.elevationMap
--         |> Canvas.group []


randomSizeGenerator : Random.Generator Int
randomSizeGenerator =
    Random.weighted ( 13, 20 )
        [ ( 16, 30 )
        , ( 20, 40 )
        , ( 22, 50 )
        , ( 21, 60 )
        , ( 20, 70 )
        , ( 17, 80 )
        , ( 15, 90 )
        , ( 13, 100 )
        , ( 8, 110 )
        , ( 3, 120 )
        ]


randomTypeGenerator : Random.Generator IslandType
randomTypeGenerator =
    Random.weighted ( 6, Vanilla ) [ ( 2, Cliffs ), ( 2, Mountains ), ( 2, Woods ) ]


maxRadius : Island -> Float
maxRadius { size } =
    toFloat size + 10
