module Island exposing (..)

import Canvas exposing (lineTo, path, shapes)
import Canvas.Settings exposing (fill, stroke)
import Hexagon exposing (GridPoint)
import Island.Elevation exposing (Elevation, Elevation2D)
import Island.IslandType exposing (IslandType(..))
import List.Extra
import Random
import Simplex


type alias Island =
    { center : GridPoint
    , size : Int
    , elevationMap : Elevation2D
    , iType : IslandType
    }


type alias IslandOptions =
    { center : GridPoint
    , size : Int
    , iType : IslandType
    , permutationTable : Simplex.PermutationTable
    }


init : IslandOptions -> Island
init { center, size, iType, permutationTable } =
    Debug.log "Island" <|
        { center = center
        , size = size
        , elevationMap = elevationFromNoise size permutationTable
        , iType = iType
        }


elevationFromNoise : Int -> Simplex.PermutationTable -> Elevation2D
elevationFromNoise size permutationTable =
    let
        maxRadius =
            size // 10 + 1

        {- currently produces numbers between 1 and 17 -}
        elevationAt x y =
            let
                ( xf, yf ) =
                    ( toFloat x, toFloat y )

                distance =
                    (xf ^ 2 + yf ^ 2) ^ (1 / 2)

                maxRadiusF =
                    toFloat maxRadius

                noiseConfig =
                    { steps = 3
                    , stepSize = 2.0
                    , persistence = 2.0
                    , scale = 2.5
                    }

                -- number between 0 and 2 scaled down further away from the center
                islandNoise =
                    (Simplex.fractal2d noiseConfig permutationTable xf yf + 1)
                        |> (*) (1 - ((distance / maxRadiusF) ^ 3))

                -- Noise values below this won't be used (elevation will be zero)
                noiseThreshold =
                    0.3

                -- How many possible elevation values are there?
                maxElevation =
                    17
            in
            if distance > maxRadiusF || islandNoise < noiseThreshold then
                0

            else
                (maxElevation * (islandNoise - noiseThreshold) / (2 - noiseThreshold))
                    -- |> (*) (1 - ((distance / maxRadiusF) ^ 3))
                    |> ceiling

        radiusList =
            List.range -(maxRadius - 1) (maxRadius - 1)
    in
    radiusList
        |> List.map
            (\x ->
                radiusList
                    |> List.map (\y -> elevationAt x y)
            )


{-| Fold elevation map into a list of renderables. Use Canvas.group to combine into one renderable.
-}
renderable : Int -> Island -> Canvas.Renderable
renderable hexSize island =
    let
        hexRenderable : GridPoint -> Elevation -> Maybe Canvas.Renderable
        hexRenderable gridPoint elevation =
            let
                hex =
                    Hexagon.hexagon hexSize gridPoint

                hexPath =
                    Tuple.second hex.corners |> List.map lineTo

                color =
                    Island.Elevation.color island.iType elevation
            in
            if elevation == 0 then
                Nothing

            else
                Just <| shapes [ stroke color, fill color ] [ path (Tuple.first hex.corners) hexPath ]

        gridPointFromIndices : Int -> Int -> GridPoint
        gridPointFromIndices i j =
            let
                ( xi, yi ) =
                    island.center

                offset =
                    island.size // 10
            in
            -- TODO: Offset by max radius (or max radius minus one) to center the island
            ( xi + i - offset, yi + j - offset )

        columnFolder : Int -> Int -> Elevation -> List Canvas.Renderable -> List Canvas.Renderable
        columnFolder i j elevation renderables =
            case hexRenderable (gridPointFromIndices i j) elevation of
                Just hex ->
                    hex :: renderables

                Nothing ->
                    renderables

        mapFolder : Int -> List Elevation -> List Canvas.Renderable -> List Canvas.Renderable
        mapFolder i columnElevation renderables =
            List.Extra.indexedFoldl (columnFolder i) renderables columnElevation
    in
    List.Extra.indexedFoldl mapFolder [] island.elevationMap
        |> Canvas.group []



-- renderableHex : Int -> Island -> Canvas.Renderable
-- renderableHex hexSize island =
--     let
--         hex =
--             hexagon hexSize island.center
--         hexPath =
--             Tuple.second hex.corners |> List.map lineTo
--         -- landHexes = island.elevationMap
--     in
--     shapes [ stroke Color.white, fill Color.green ] [ path (Tuple.first hex.corners) hexPath ]


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
    -- Random.uniform Vanilla [ Cliffs, Mountains, Woods ]
    Random.weighted ( 6, Vanilla ) [ ( 2, Cliffs ), ( 2, Mountains ), ( 2, Woods ) ]
