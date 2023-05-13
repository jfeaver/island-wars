module Main exposing (..)

import Browser
import Camera exposing (Camera2D)
import Canvas.Settings.Text exposing (TextAlign(..))
import Html exposing (button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Island.IslandType exposing (IslandType)
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



--- UPDATE


setGameZoom : Game -> Float -> Game
setGameZoom game newZoom =
    let
        zoomedCamera =
            Camera.setZoom game.worldCamera newZoom
    in
    { game | worldCamera = zoomedCamera }


setGameFocus : Game -> Float -> Game
setGameFocus game focus =
    let
        world =
            game.world

        focusedWorld =
            { world | focus = focus }
    in
    { game | world = focusedWorld }


setGameSize : Game -> Int -> Game
setGameSize game size =
    let
        island =
            game.world.island

        world =
            game.world

        sizedIsland =
            { island | size = size }
    in
    { game | world = { world | island = sizedIsland, islands = [ sizedIsland ] } }


setIslandType : Game -> IslandType -> Game
setIslandType game iType =
    let
        island =
            game.world.island

        world =
            game.world

        typedIsland =
            { island | iType = iType }
    in
    { game | world = { world | island = typedIsland, islands = [ typedIsland ] } }


type Msg
    = StartGame
    | SeedWorld World.Seed
    | WorldMsg World.Msg
    | CameraZoom String
    | WorldFocus String
    | UpdateIslandSize String
    | UpdateIslandType String


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
                            ( InGame <| setGameZoom game zoom, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        WorldFocus text ->
            case model of
                InGame game ->
                    let
                        mFocus =
                            String.toFloat text
                    in
                    case mFocus of
                        Just focus ->
                            ( InGame <| setGameFocus game focus, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateIslandSize text ->
            case model of
                InGame game ->
                    let
                        mSize =
                            String.toInt text
                    in
                    case mSize of
                        Just size ->
                            ( InGame <| setGameSize game size, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateIslandType text ->
            case model of
                InGame game ->
                    let
                        mType =
                            Island.IslandType.fromString text
                    in
                    case mType of
                        Just iType ->
                            ( InGame <| setIslandType game iType, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
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
                            [ Html.div []
                                [ Html.input
                                    [ Html.Attributes.type_ "range"
                                    , Html.Attributes.min "0.1"
                                    , Html.Attributes.max "5"
                                    , Html.Attributes.step "0.1"
                                    , Html.Attributes.value <| String.fromFloat (Camera.getZoom game.worldCamera)
                                    , Html.Events.onInput CameraZoom
                                    ]
                                    []
                                , Html.br [] []
                                , Html.label [] [ Html.text ("Zoom: " ++ String.fromFloat (Camera.getZoom game.worldCamera)) ]
                                ]
                            , Html.div []
                                [ Html.input
                                    [ Html.Attributes.type_ "range"
                                    , Html.Attributes.min "0.1"
                                    , Html.Attributes.max "5"
                                    , Html.Attributes.step "0.1"
                                    , Html.Attributes.value <| String.fromFloat game.world.focus
                                    , Html.Events.onInput WorldFocus
                                    ]
                                    []
                                , Html.br [] []
                                , Html.label [] [ Html.text ("Focus: " ++ String.fromFloat game.world.focus) ]
                                ]
                            , Html.div []
                                [ Html.input
                                    [ Html.Attributes.type_ "range"
                                    , Html.Attributes.min "20"
                                    , Html.Attributes.max "150"
                                    , Html.Attributes.step "10"
                                    , Html.Attributes.value <| String.fromInt game.world.island.size
                                    , Html.Events.onInput UpdateIslandSize
                                    ]
                                    []
                                , Html.br [] []
                                , Html.label [] [ Html.text ("Size: " ++ String.fromInt game.world.island.size) ]
                                ]
                            , Html.div [ Html.Attributes.class "inline-radio-selector" ]
                                [ Html.div []
                                    [ Html.input
                                        [ Html.Attributes.type_ "radio"
                                        , Html.Attributes.id "iType-typical"
                                        , Html.Attributes.name "iType"
                                        , Html.Attributes.value "typical"
                                        , Html.Attributes.checked (game.world.island.iType == Island.IslandType.Vanilla)
                                        , Html.Events.onInput UpdateIslandType
                                        ]
                                        []
                                    , Html.br [] []
                                    , Html.label [ Html.Attributes.for "iType-typical" ] [ Html.text "Typical" ]
                                    ]
                                , Html.div []
                                    [ Html.input
                                        [ Html.Attributes.type_ "radio"
                                        , Html.Attributes.id "iType-woods"
                                        , Html.Attributes.name "iType"
                                        , Html.Attributes.value "woods"
                                        , Html.Attributes.checked (game.world.island.iType == Island.IslandType.Woods)
                                        , Html.Events.onInput UpdateIslandType
                                        ]
                                        []
                                    , Html.br [] []
                                    , Html.label [ Html.Attributes.for "iType-woods" ] [ Html.text "Woods" ]
                                    ]
                                , Html.div []
                                    [ Html.input
                                        [ Html.Attributes.type_ "radio"
                                        , Html.Attributes.id "iType-cliffs"
                                        , Html.Attributes.name "iType"
                                        , Html.Attributes.value "cliffs"
                                        , Html.Attributes.checked (game.world.island.iType == Island.IslandType.Cliffs)
                                        , Html.Events.onInput UpdateIslandType
                                        ]
                                        []
                                    , Html.br [] []
                                    , Html.label [ Html.Attributes.for "iType-cliffs" ] [ Html.text "Cliffs" ]
                                    ]
                                , Html.div []
                                    [ Html.input
                                        [ Html.Attributes.type_ "radio"
                                        , Html.Attributes.id "iType-mountains"
                                        , Html.Attributes.name "iType"
                                        , Html.Attributes.value "mountains"
                                        , Html.Attributes.checked (game.world.island.iType == Island.IslandType.Mountains)
                                        , Html.Events.onInput UpdateIslandType
                                        ]
                                        []
                                    , Html.br [] []
                                    , Html.label [ Html.Attributes.for "iType-mountains" ] [ Html.text "Mountains" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
    in
    { title = "Island Wars"
    , body = body
    }
