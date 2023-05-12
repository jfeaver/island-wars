module Hexagon exposing (..)


type alias Size =
    Float


hexUnitPoints : ( ( Float, Float ), List ( Float, Float ) )
hexUnitPoints =
    let
        root3Over2 =
            3 ^ 0.5 / 2
    in
    ( ( 1, 0 ), [ ( 0.5, root3Over2 ), ( -0.5, root3Over2 ), ( -1, 0 ), ( -0.5, -root3Over2 ), ( 0.5, -root3Over2 ), ( 1, 0 ) ] )


{-| Grid point to grid point (vertical spacing for flat tops)
-}
gridSpacing : Size -> Float
gridSpacing =
    verticalSpacing


verticalSpacing : Size -> Float
verticalSpacing size =
    size * sqrt 3


horizontalSpacing : Size -> Float
horizontalSpacing size =
    3 * size / 2


corners : Float -> ( Float, Float ) -> ( ( Float, Float ), List ( Float, Float ) )
corners hexSizeF ( x, y ) =
    let
        unitToCornerMapper =
            Tuple.mapBoth (\cosTheta -> x + hexSizeF * cosTheta) (\sinTheta -> y + hexSizeF * sinTheta)
    in
    Tuple.mapBoth unitToCornerMapper (List.map unitToCornerMapper) hexUnitPoints
