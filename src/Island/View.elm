module Island.View exposing (RenderableConfig, renderable)

import Camera exposing (Camera2D)
import Canvas exposing (lineTo, path, shapes)
import Canvas.Settings exposing (fill, stroke)
import Coordinate exposing (Coordinate)
import Hexagon
import Island exposing (Island)
import Island.Elevation
import Point exposing (Point)
import Simplex


type alias RenderableConfig =
    { camera : Camera2D
    , worldHexSize : Float
    , focus : Float
    , noiseConfig : Simplex.FractalConfig
    , permutationTable : Simplex.PermutationTable
    }


type alias IslandHexagon =
    { center : Point
    , corners : ( Point, List Point )
    , fade : Float -- the fraction towards the beach it is
    }


renderable : RenderableConfig -> Island -> Canvas.Renderable
renderable config island =
    let
        { camera, worldHexSize, focus, noiseConfig, permutationTable } =
            config

        loc =
            Camera.locate camera

        -- px units
        hexSize =
            Camera.distance camera worldHexSize / focus

        -- world units
        maxRadius =
            Island.maxRadius island

        relativeGridLocations : List Int
        relativeGridLocations =
            let
                maxGridRadius =
                    maxRadius * focus / Hexagon.gridSpacing worldHexSize |> ceiling
            in
            List.range -maxGridRadius maxGridRadius

        allHexagonRelativeLocations : List Coordinate
        allHexagonRelativeLocations =
            List.foldl
                (\xgr soFar ->
                    relativeGridLocations
                        |> List.foldl (\ygr soFarNested -> ( xgr, ygr ) :: soFarNested) []
                        |> List.append soFar
                )
                []
                relativeGridLocations

        allHexagonWorldLocations : List Point
        allHexagonWorldLocations =
            let
                ( xc, yc ) =
                    island.center

                vert =
                    Hexagon.verticalSpacing (worldHexSize / focus)

                relToWorld ( xr, yr ) =
                    let
                        vertShove =
                            if modBy 2 xr == 1 then
                                vert / 2

                            else
                                0
                    in
                    ( toFloat xr * Hexagon.horizontalSpacing (worldHexSize / focus) + xc
                    , toFloat yr * vert + vertShove + yc
                    )
            in
            List.map relToWorld allHexagonRelativeLocations

        allHexagons : List IslandHexagon
        allHexagons =
            let
                removeHexagonsBeyondMaxRadius ( xc, yc ) ( x, y ) soFar =
                    let
                        radiusSquared =
                            (x - xc) ^ 2 + (y - yc) ^ 2

                        maxRadiusSquared =
                            maxRadius ^ 2
                    in
                    if radiusSquared < maxRadiusSquared then
                        { center = ( x, y )
                        , corners = Hexagon.corners hexSize (loc ( x, y ))
                        , fade = radiusSquared / maxRadiusSquared
                        }
                            :: soFar

                    else
                        soFar
            in
            allHexagonWorldLocations
                |> List.foldl (removeHexagonsBeyondMaxRadius island.center) []

        elevationAt ( x, y ) fade =
            let
                rawNoise =
                    Simplex.fractal2d noiseConfig permutationTable x y

                positiveNoise =
                    rawNoise + 1

                -- Noise values below this won't be used (elevation will be zero)
                noiseThreshold =
                    0.7

                fadeMultiplier =
                    1

                lumpinessFactor =
                    1.5

                fadedNoise =
                    positiveNoise * fadeMultiplier * (1 - (fade ^ lumpinessFactor))

                thresholdedNoise =
                    if fadedNoise < noiseThreshold then
                        0

                    else
                        (fadedNoise - noiseThreshold) / (2 - noiseThreshold)

                -- How many possible elevation values are there?
                maxElevation =
                    22
            in
            thresholdedNoise
                * maxElevation
                |> ceiling

        hexagonRenderable { center, corners, fade } =
            let
                ( firstPoint, others ) =
                    corners

                elevation =
                    elevationAt center fade

                color =
                    Island.Elevation.color island elevation
            in
            shapes [ stroke color, fill color ]
                [ path
                    firstPoint
                    (others |> List.map lineTo)
                ]

        renderables =
            List.map hexagonRenderable allHexagons
    in
    Canvas.group [] renderables
