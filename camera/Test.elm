module Test exposing (..)

import Browser exposing (Document)
import Camera.Utils
import CameraTest exposing (Camera2D)
import Html exposing (Html)
import Html.Attributes exposing (id, style)
import Html.Events


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { camera : Camera2D }


init : () -> ( Model, Cmd Msg )
init =
    always ( { camera = CameraTest.init ( 200, 200 ) }, Cmd.none )


type Msg
    = CameraZoom String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CameraZoom text ->
            let
                mZoom =
                    String.toFloat text
            in
            case mZoom of
                Just zoom ->
                    ( { model | camera = CameraTest.setZoom model.camera zoom }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


pxf : Float -> String
pxf n =
    String.fromFloat n ++ "px"


px : Int -> String
px n =
    String.fromInt n ++ "px"


{-| All in world units
-}
type alias Box =
    { center : ( Float, Float )
    , size : Float
    }


testBox : Camera2D -> Box -> Html Msg
testBox camera box =
    let
        pxSize =
            CameraTest.distance camera box.size

        ( x, y ) =
            CameraTest.locate camera box.center
                |> Tuple.mapBoth (Camera.Utils.center pxSize) (Camera.Utils.center pxSize)
    in
    Html.span
        [ style "display" "inline-block"
        , style "position" "relative"
        , style "top" (pxf x)
        , style "left" (pxf y)
        , style "height" (pxf pxSize)
        , style "width" (pxf pxSize)
        , style "background-color" "purple"
        ]
        []


view : Model -> Document Msg
view model =
    let
        ( height, width ) =
            CameraTest.getViewport model.camera
    in
    { title = ""
    , body =
        [ Html.div [ id "camera-viewport", style "height" (px height), style "width" (px width) ]
            [ testBox model.camera { center = ( 0, 0 ), size = 20 } ]
        , Html.div [ id "camera-controls" ]
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "0.1"
                , Html.Attributes.max "3"
                , Html.Attributes.step "0.1"
                , Html.Attributes.value <| String.fromFloat (CameraTest.getZoom model.camera)
                , Html.Events.onInput CameraZoom
                ]
                []
            , Html.br [] []
            , Html.label [] [ Html.text "Zoom" ]
            ]
        ]
    }
