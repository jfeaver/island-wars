module Island.LandType exposing (..)

import Color


type LandType
    = Ocean
    | Beach1
    | Beach2
    | Grass1
    | Grass2
    | Trees1
    | Trees2
    | Trees3
    | Mountain1
    | Mountain2
    | Stones1
    | Stones2
    | Stones3
    | Cliffs1
    | Cliffs2


toString : LandType -> String
toString landType =
    case landType of
        Ocean ->
            "ocean"

        Beach1 ->
            "beach1"

        Beach2 ->
            "beach2"

        Grass1 ->
            "grass1"

        Grass2 ->
            "grass2"

        Trees1 ->
            "trees1"

        Trees2 ->
            "trees2"

        Trees3 ->
            "trees3"

        Mountain1 ->
            "mountain1"

        Mountain2 ->
            "mountain2"

        Stones1 ->
            "stones1"

        Stones2 ->
            "stones2"

        Stones3 ->
            "stones3"

        Cliffs1 ->
            "cliffs1"

        Cliffs2 ->
            "cliffs2"



-- #004080
-- #F9D332
-- #F9EC32
-- #56C936
-- #4DB231
-- #3A8625
-- #C4E0E0
-- #F1FFFF
-- #287114
-- #145802
-- #7D736B
-- #71645B
-- #66594F
-- #E8E0D9
-- #F2F2EE


toColor : LandType -> Color.Color
toColor landType =
    case landType of
        Ocean ->
            Color.rgb255 0 64 128

        Beach1 ->
            Color.rgb255 249 211 50

        Beach2 ->
            Color.rgb255 249 236 50

        Grass1 ->
            Color.rgb255 86 201 54

        Grass2 ->
            Color.rgb255 77 178 49

        Trees1 ->
            Color.rgb255 58 134 37

        Trees2 ->
            Color.rgb255 40 113 20

        Trees3 ->
            Color.rgb255 20 88 2

        Mountain1 ->
            Color.rgb255 196 224 224

        Mountain2 ->
            Color.rgb255 241 255 255

        Stones1 ->
            Color.rgb255 125 115 107

        Stones2 ->
            Color.rgb255 113 100 91

        Stones3 ->
            Color.rgb255 102 89 79

        Cliffs1 ->
            Color.rgb255 232 224 217

        Cliffs2 ->
            Color.rgb255 242 242 238
