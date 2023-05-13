module Editor exposing (..)

import Browser
import Camera exposing (Camera2D)
import Canvas.Settings.Text exposing (TextAlign(..))
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Island.IslandType exposing (IslandType)
import Random
import Simplex
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
    , worldCamera = Camera.init ( 1400, 600 )
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
    | Regenerate
    | RegenerateHelper Simplex.PermutationTable


gameUpdate : Model -> (Game -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
gameUpdate model updater =
    case model of
        InGame game ->
            updater game

        _ ->
            ( model, Cmd.none )


updateFromInput : Model -> String -> (String -> Maybe a) -> (Game -> a -> Game) -> ( Model, Cmd Msg )
updateFromInput model inputText inputParser gameUpdater =
    gameUpdate model
        (\game ->
            let
                mInput =
                    inputParser inputText
            in
            case mInput of
                Just someInput ->
                    ( InGame <| gameUpdater game someInput, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( model, Random.generate SeedWorld World.seed )

        SeedWorld worldSeed ->
            ( InGame (gameInit worldSeed), Cmd.none )

        WorldMsg worldMsg ->
            gameUpdate model
                (\game ->
                    let
                        ( updatedWorld, cmd ) =
                            World.update worldMsg game.world

                        mainCmd =
                            Cmd.map WorldMsg cmd
                    in
                    ( InGame { game | world = updatedWorld }, mainCmd )
                )

        CameraZoom text ->
            updateFromInput model text String.toFloat setGameZoom

        WorldFocus text ->
            updateFromInput model text String.toFloat setGameFocus

        UpdateIslandSize text ->
            updateFromInput model text String.toInt setGameSize

        UpdateIslandType text ->
            updateFromInput model text Island.IslandType.fromString setIslandType

        Regenerate ->
            ( model, Random.generate RegenerateHelper Simplex.permutationTableGenerator )

        RegenerateHelper permutationTable ->
            gameUpdate model
                (\game ->
                    let
                        world =
                            game.world

                        updatedWorld =
                            { world | permutationTable = permutationTable }

                        updatedGame =
                            { game | world = updatedWorld }
                    in
                    ( InGame updatedGame, Cmd.none )
                )



--- VIEW
-- TODO:
-- 2. Add inputs for noise threshold, fadeMultiplier, lumpinessFactor
-- 3. Figure out itch.io requirements
-- 4. Build and upload


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
                        , editorControls game
                        ]
                    ]
    in
    { title = "Island Wars"
    , body = body
    }


editorControls : Game -> Html Msg
editorControls game =
    Html.div [ Html.Attributes.class "editor-controls" ]
        [ editorInputs game
        , Html.div [] [ Html.button [ Html.Events.onClick Regenerate ] [ text "Regenerate" ] ]
        ]


editorInputs : Game -> Html Msg
editorInputs game =
    Html.div []
        [ Html.div []
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "0.1"
                , Html.Attributes.max "10"
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
                , Html.Attributes.max "8"
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
