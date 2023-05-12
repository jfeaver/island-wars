module Point exposing (..)

import Coordinate exposing (Coordinate)


type alias Point =
    ( Float, Float )


fromCoordinate : Coordinate -> Point
fromCoordinate =
    Tuple.mapBoth toFloat toFloat
