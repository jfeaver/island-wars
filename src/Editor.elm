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


setOceanElevation : Game -> Float -> Game
setOceanElevation game elevation =
    let
        world =
            game.world

        generationConfig =
            world.generationConfig

        updatedGenerationConfig =
            { generationConfig | oceanElevation = elevation }

        updatedWorld =
            { world | generationConfig = updatedGenerationConfig }
    in
    { game | world = updatedWorld }


setElevationScalar : Game -> Float -> Game
setElevationScalar game elevationScalar =
    let
        world =
            game.world

        generationConfig =
            world.generationConfig

        updatedGenerationConfig =
            { generationConfig | elevationScalar = elevationScalar }

        updatedWorld =
            { world | generationConfig = updatedGenerationConfig }
    in
    { game | world = updatedWorld }


setCircleFactor : Game -> Float -> Game
setCircleFactor game circleFactor =
    let
        world =
            game.world

        generationConfig =
            world.generationConfig

        updatedGenerationConfig =
            { generationConfig | circleFactor = circleFactor }

        updatedWorld =
            { world | generationConfig = updatedGenerationConfig }
    in
    { game | world = updatedWorld }


setNoiseSteps : Game -> Int -> Game
setNoiseSteps game steps =
    let
        world =
            game.world

        noiseConfig =
            world.noiseConfig

        updatedNoiseConfig =
            { noiseConfig | steps = steps }

        updatedWorld =
            { world | noiseConfig = updatedNoiseConfig }
    in
    { game | world = updatedWorld }


setNoisePersistence : Game -> Float -> Game
setNoisePersistence game persistence =
    let
        world =
            game.world

        noiseConfig =
            world.noiseConfig

        updatedNoiseConfig =
            { noiseConfig | persistence = persistence }

        updatedWorld =
            { world | noiseConfig = updatedNoiseConfig }
    in
    { game | world = updatedWorld }


setNoiseScale : Game -> Float -> Game
setNoiseScale game scale =
    let
        world =
            game.world

        noiseConfig =
            world.noiseConfig

        updatedNoiseConfig =
            { noiseConfig | scale = scale * World.hexSize }

        updatedWorld =
            { world | noiseConfig = updatedNoiseConfig }
    in
    { game | world = updatedWorld }


setNoiseStepSize : Game -> Float -> Game
setNoiseStepSize game stepSize =
    let
        world =
            game.world

        noiseConfig =
            world.noiseConfig

        updatedNoiseConfig =
            { noiseConfig | stepSize = stepSize }

        updatedWorld =
            { world | noiseConfig = updatedNoiseConfig }
    in
    { game | world = updatedWorld }


type Msg
    = StartGame
    | SeedWorld World.Seed
    | WorldMsg World.Msg
    | CameraZoom String
    | WorldFocus String
    | UpdateIslandSize String
    | UpdateIslandType String
    | OceanElevation String
    | CircleFactor String
    | ElevationScalar String
    | NoiseSteps String
    | NoiseStepSize String
    | Persistence String
    | Scale String
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

        OceanElevation text ->
            updateFromInput model text String.toFloat setOceanElevation

        ElevationScalar text ->
            updateFromInput model text String.toFloat setElevationScalar

        CircleFactor text ->
            updateFromInput model text String.toFloat setCircleFactor

        NoiseSteps text ->
            updateFromInput model text String.toInt setNoiseSteps

        NoiseStepSize text ->
            updateFromInput model text String.toFloat setNoiseStepSize

        Persistence text ->
            updateFromInput model text String.toFloat setNoisePersistence

        Scale text ->
            updateFromInput model text String.toFloat setNoiseScale

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
-- 3. Figure out itch.io requirements
-- 4. Build and upload
-- 5. Add inputs for noise config?


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
        , noiseInputs game
        , advancedInputs game
        , Html.div [] [ Html.button [ Html.Events.onClick Regenerate ] [ text "Regenerate" ] ]
        ]


advancedInputs : Game -> Html Msg
advancedInputs game =
    let
        config =
            game.world.generationConfig
    in
    Html.div []
        [ Html.div []
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "0.0"
                , Html.Attributes.max "1.5"
                , Html.Attributes.step "0.05"
                , Html.Attributes.value <| String.fromFloat config.oceanElevation
                , Html.Events.onInput OceanElevation
                ]
                []
            , Html.br [] []
            , Html.label [] [ Html.text ("Ocean Elevation: " ++ String.fromFloat config.oceanElevation) ]
            ]
        , Html.div []
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "0.0"
                , Html.Attributes.max "7.0"
                , Html.Attributes.step "0.25"
                , Html.Attributes.value <| String.fromFloat config.circleFactor
                , Html.Events.onInput CircleFactor
                ]
                []
            , Html.br [] []
            , Html.label [] [ Html.text ("Circle Factor: " ++ String.fromFloat config.circleFactor) ]
            ]
        , Html.div []
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "0.1"
                , Html.Attributes.max "1.5"
                , Html.Attributes.step "0.1"
                , Html.Attributes.value <| String.fromFloat config.elevationScalar
                , Html.Events.onInput ElevationScalar
                ]
                []
            , Html.br [] []
            , Html.label [] [ Html.text ("Elevation Scalar: " ++ String.fromFloat config.elevationScalar) ]
            ]
        ]


noiseInputs : Game -> Html Msg
noiseInputs game =
    let
        config =
            game.world.noiseConfig
    in
    Html.div []
        [ Html.div []
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "1"
                , Html.Attributes.max "5"
                , Html.Attributes.step "1"
                , Html.Attributes.value <| String.fromInt config.steps
                , Html.Events.onInput NoiseSteps
                ]
                []
            , Html.br [] []
            , Html.label [] [ Html.text ("Noise Steps: " ++ String.fromInt config.steps) ]
            ]
        , Html.div []
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "1"
                , Html.Attributes.max "3.5"
                , Html.Attributes.step "0.1"
                , Html.Attributes.value <| String.fromFloat config.stepSize
                , Html.Events.onInput NoiseStepSize
                ]
                []
            , Html.br [] []
            , Html.label [] [ Html.text ("Noise Step Size: " ++ String.fromFloat config.stepSize) ]
            ]
        , Html.div []
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "0.0"
                , Html.Attributes.max "5"
                , Html.Attributes.step "0.25"
                , Html.Attributes.value <| String.fromFloat config.persistence
                , Html.Events.onInput Persistence
                ]
                []
            , Html.br [] []
            , Html.label [] [ Html.text ("Noise Persistence: " ++ String.fromFloat config.persistence) ]
            ]
        , Html.div []
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "0.0"
                , Html.Attributes.max "4"
                , Html.Attributes.step "0.1"
                , Html.Attributes.value <| String.fromFloat (config.scale / World.hexSize)
                , Html.Events.onInput Scale
                ]
                []
            , Html.br [] []
            , Html.label [] [ Html.text ("Noise Scale: " ++ String.fromFloat (config.scale / World.hexSize)) ]
            ]
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
