module Camera exposing (Camera2D, distance, getViewport, getZoom, init, locate, setZoom)

import Point exposing (Point)


type Camera2D
    = Camera2D Camera2DR


type alias Camera2DR =
    { viewport : ( Int, Int )
    , zoom : Float
    , center : Point
    }


init : ( Int, Int ) -> Camera2D
init viewport =
    { viewport = viewport
    , zoom = 1
    , center = ( 0, 0 )
    }
        |> Camera2D


{-| World point to viewport point conversion

subject: 0, 0
camera center: 100, 100
camera zoom: 1
viewport: 200, 200
should return 0, 0

increase zoom to 2
should return -100, -100

-}
locate : Camera2D -> Point -> Point
locate (Camera2D camera) subjectPoint =
    let
        ( xs, ys ) =
            subjectPoint

        ( xv, yv ) =
            Point.fromCoordinate camera.viewport

        ( xc, yc ) =
            camera.center
    in
    ( (xs - xc) * camera.zoom + xv / 2, (ys - yc) * camera.zoom + yv / 2 )


getViewport : Camera2D -> ( Int, Int )
getViewport (Camera2D { viewport }) =
    viewport


getZoom : Camera2D -> Float
getZoom (Camera2D { zoom }) =
    zoom


setZoom : Camera2D -> Float -> Camera2D
setZoom (Camera2D camera) zoom =
    { camera | zoom = zoom } |> Camera2D


distance : Camera2D -> Float -> Float
distance (Camera2D { zoom }) d =
    d * zoom
