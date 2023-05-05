module Island.Elevation exposing (..)

import Color
import Island.IslandType exposing (IslandType(..))
import Island.LandType as LandType exposing (LandType(..))


type alias Elevation =
    Int


type alias Elevation2D =
    List (List Elevation)


landType : IslandType -> Elevation -> LandType
landType iType elevation =
    let
        vanillaLands =
            if elevation < 2 then
                Beach1

            else if elevation < 4 then
                Beach2

            else if elevation < 8 then
                Grass1

            else if elevation < 10 then
                Grass2

            else if elevation < 14 then
                Trees1

            else if elevation < 17 then
                Mountain1

            else
                Mountain2

        mountainsLands =
            if elevation < 3 then
                Beach1

            else if elevation < 5 then
                Beach2

            else if elevation < 8 then
                Stones1

            else if elevation < 11 then
                Stones2

            else if elevation < 13 then
                Stones3

            else if elevation < 16 then
                Mountain1

            else
                Mountain2

        woodsLands =
            if elevation < 3 then
                Beach1

            else if elevation < 8 then
                Trees1

            else if elevation < 9 then
                Trees2

            else if elevation < 17 then
                Trees3

            else
                Mountain1

        cliffsLands =
            if elevation < 3 then
                Cliffs1

            else if elevation < 4 then
                Cliffs2

            else if elevation < 11 then
                Grass1

            else if elevation < 16 then
                Trees1

            else if elevation < 17 then
                Mountain1

            else
                Mountain2
    in
    if elevation == 0 then
        Ocean

    else
        case iType of
            Vanilla ->
                vanillaLands

            Mountains ->
                mountainsLands

            Woods ->
                woodsLands

            Cliffs ->
                cliffsLands


color : IslandType -> Elevation -> Color.Color
color iType elevation =
    landType iType elevation
        |> LandType.toColor
