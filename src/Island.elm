module Island exposing (..)

-- import GridPoint exposing (GridPoint)

import Island.IslandType exposing (IslandType(..))
import Point exposing (Point)
import Random


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
