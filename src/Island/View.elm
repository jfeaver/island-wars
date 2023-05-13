module Island.View exposing (..)

import Camera exposing (Camera2D)
import Canvas exposing (lineTo, path, shapes)
import Canvas.Settings exposing (fill, stroke)
import Color
import Coordinate exposing (Coordinate)
import Hexagon
import Island exposing (Island)
import Point exposing (Point)


renderable : Camera2D -> Float -> Island -> Canvas.Renderable
renderable camera focus island =
    let
        loc =
            Camera.locate camera

        worldHexSize =
            10

        -- px units
        hexSize =
            Camera.distance camera worldHexSize
                / focus

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

        allHexagons : List ( Canvas.Point, List Canvas.Point )
        allHexagons =
            let
                removeHexagonsBeyondMaxRadius ( xc, yc ) ( x, y ) soFar =
                    if (x - xc) ^ 2 + (y - yc) ^ 2 < maxRadius ^ 2 then
                        Hexagon.corners hexSize (loc ( x, y )) :: soFar

                    else
                        soFar
            in
            allHexagonWorldLocations
                |> List.foldl (removeHexagonsBeyondMaxRadius island.center) []

        hexagonRenderable ( firstPoint, others ) =
            shapes [ stroke Color.darkPurple, fill Color.purple ]
                [ path
                    firstPoint
                    (others |> List.map lineTo)
                ]

        renderables =
            List.map hexagonRenderable allHexagons
    in
    Canvas.group [] renderables
