module Island.Elevation exposing (..)

import Color
import Dict
import Island.IslandType as IslandType exposing (IslandType(..))


type alias Elevation =
    Int


type alias Elevation2D =
    List (List Elevation)


color : IslandType -> Elevation -> Color.Color
color iType elevation =
    let
        vanillaDict : Dict.Dict Elevation Color.Color
        vanillaDict =
            Dict.fromList
                [ ( 1, Color.hsl (50 / 360) (90 / 100) (60 / 100) )
                , ( 2, Color.hsl (65 / 360) (80 / 100) (60 / 100) )
                , ( 3, Color.hsl (80 / 360) (55 / 100) (60 / 100) )
                , ( 4, Color.hsl (87 / 360) (50 / 100) (60 / 100) )
                , ( 5, Color.hsl (92 / 360) (62 / 100) (60 / 100) )
                , ( 6, Color.hsl (95 / 360) (75 / 100) (60 / 100) )
                , ( 7, Color.hsl (98 / 360) (90 / 100) (60 / 100) )
                , ( 8, Color.hsl (99 / 360) (90 / 100) (60 / 100) )
                , ( 9, Color.hsl (100 / 360) (90 / 100) (60 / 100) )
                , ( 10, Color.hsl (100 / 360) (90 / 100) (60 / 100) )
                , ( 11, Color.hsl (100 / 360) (90 / 100) (60 / 100) )
                , ( 12, Color.hsl (220 / 360) (10 / 100) (90 / 100) )
                , ( 13, Color.hsl (220 / 360) (10 / 100) (90 / 100) )
                , ( 14, Color.hsl (220 / 360) (10 / 100) (90 / 100) )
                , ( 15, Color.hsl (220 / 360) (10 / 100) (97 / 100) )
                , ( 16, Color.hsl (220 / 360) (10 / 100) (97 / 100) )
                , ( 17, Color.hsl (220 / 360) (10 / 100) (97 / 100) )
                ]

        colorDict =
            Dict.fromList
                [ ( IslandType.toString Vanilla, vanillaDict )
                ]
    in
    Dict.get (IslandType.toString iType) colorDict
        |> Maybe.andThen (Dict.get elevation)
        |> Maybe.withDefault Color.purple
