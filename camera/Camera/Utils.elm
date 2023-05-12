module Camera.Utils exposing (..)

{-| No documentation
-}


{-| This will center square objects but not points of a hexagon which would
--| need to consider the angle (but this is already considered in
--| Hexagon.corners)
-}
center : Float -> Float -> Float
center distance objectCenter =
    objectCenter - distance / 2
