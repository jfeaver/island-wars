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
            Color.rgb255 0x00 0x40 0x80

        Beach1 ->
            Color.rgb255 0xF9 0xD3 0x32

        Beach2 ->
            Color.rgb255 0xF9 0xEC 0x32

        Grass1 ->
            Color.rgb255 0x56 0xC9 0x36

        Grass2 ->
            Color.rgb255 0x4D 0xB2 0x31

        Trees1 ->
            Color.rgb255 0x3A 0x86 0x25

        Trees2 ->
            Color.rgb255 0x28 0x71 0x14

        Trees3 ->
            Color.rgb255 0x14 0x58 0x02

        Mountain1 ->
            Color.rgb255 0xC4 0xE0 0xE0

        Mountain2 ->
            Color.rgb255 0xF1 0xFF 0xFF

        Stones1 ->
            Color.rgb255 0x7D 0x73 0x6B

        Stones2 ->
            Color.rgb255 0x71 0x64 0x5B

        Stones3 ->
            Color.rgb255 0x66 0x59 0x4F

        Cliffs1 ->
            Color.rgb255 0xE8 0xE0 0xD9

        Cliffs2 ->
            Color.rgb255 0xF2 0xF2 0xEE
