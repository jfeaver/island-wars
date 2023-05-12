module Main exposing (..)

import Browser
import Camera exposing (Camera2D)
import Canvas.Settings.Text exposing (TextAlign(..))
import Html exposing (button, div, text)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Random
import World exposing (World)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



--- MODEL


type Model
    = MainMenu
    | InGame Game


type alias Game =
    { world : World
    , worldCamera : Camera2D
    }


init : () -> ( Model, Cmd Msg )
init =
    always <| update StartGame MainMenu


gameInit : World.Seed -> Game
gameInit worldSeed =
    { world = World.init worldSeed
    , worldCamera = Camera.init ( 1000, 500 )
    }


gameZoom : Game -> Float -> Game
gameZoom game newZoom =
    let
        zoomedCamera =
            Camera.setZoom game.worldCamera newZoom
    in
    { game | worldCamera = zoomedCamera }



--- UPDATE


type Msg
    = StartGame
    | SeedWorld World.Seed
    | WorldMsg World.Msg
    | CameraZoom String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( model, Random.generate SeedWorld World.seed )

        SeedWorld worldSeed ->
            ( InGame (gameInit worldSeed), Cmd.none )

        WorldMsg worldMsg ->
            case model of
                InGame game ->
                    let
                        ( updatedWorld, cmd ) =
                            World.update worldMsg game.world

                        mainCmd =
                            Cmd.map WorldMsg cmd
                    in
                    ( InGame { game | world = updatedWorld }, mainCmd )

                MainMenu ->
                    ( model, Cmd.none )

        CameraZoom text ->
            case model of
                InGame game ->
                    let
                        mZoom =
                            String.toFloat text
                    in
                    case mZoom of
                        Just zoom ->
                            ( InGame <| gameZoom game zoom, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                MainMenu ->
                    ( model, Cmd.none )



--- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        body =
            case model of
                MainMenu ->
                    [ div [] [ button [ onClick StartGame ] [ text "New Game" ] ]
                    ]

                InGame game ->
                    [ Html.div [ style "text-align" "center" ]
                        [ World.view game.world game.worldCamera |> Html.map WorldMsg
                        , Html.div []
                            [ Html.input
                                [ type_ "range"
                                , Html.Attributes.min "0.1"
                                , Html.Attributes.max "3"
                                , Html.Attributes.step "0.1"
                                , value <| String.fromFloat (Camera.getZoom game.worldCamera)
                                , onInput CameraZoom
                                ]
                                []
                            , Html.br [] []
                            , Html.label [] [ Html.text "Zoom" ]
                            ]
                        ]
                    ]
    in
    { title = "Island Wars"
    , body = body
    }
