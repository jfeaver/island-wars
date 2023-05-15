module Editor exposing (..)

import Browser
import Camera exposing (Camera2D)
import Canvas.Settings.Text exposing (TextAlign(..))
import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Html.Events
import Island.IslandType exposing (IslandType(..))
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


type alias Model =
    { world : World
    , worldCamera : Camera2D
    , pendingSeed : Maybe Int
    }


init : () -> ( Model, Cmd Msg )
init =
    always <|
        update Regenerate
            { world = World.static
            , worldCamera = Camera.init ( 1400, 600 )
            , pendingSeed = Nothing
            }



--- UPDATE


setGameZoom : Model -> Float -> Model
setGameZoom game newZoom =
    let
        zoomedCamera =
            Camera.setZoom game.worldCamera newZoom
    in
    { game | worldCamera = zoomedCamera }


setGameFocus : Model -> Float -> Model
setGameFocus game focus =
    let
        world =
            game.world

        focusedWorld =
            { world | focus = focus }
    in
    { game | world = focusedWorld }


setGameSize : Model -> Int -> Model
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


setIslandType : Model -> IslandType -> Model
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


setOceanElevation : Model -> Float -> Model
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


setElevationScalar : Model -> Float -> Model
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


setCircleFactor : Model -> Float -> Model
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


setNoiseSteps : Model -> Int -> Model
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


setNoisePersistence : Model -> Float -> Model
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


setNoiseScale : Model -> Float -> Model
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


setNoiseStepSize : Model -> Float -> Model
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
    = WorldMsg World.Msg
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
    | PendingSeed String
    | UserUpdateSeed
    | Regenerate
    | UpdateSeed Int


updateFromInput : Model -> String -> (String -> Maybe a) -> (Model -> a -> Model) -> ( Model, Cmd Msg )
updateFromInput model inputText inputParser gameUpdater =
    let
        mInput =
            inputParser inputText
    in
    case mInput of
        Just someInput ->
            ( gameUpdater model someInput, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WorldMsg worldMsg ->
            let
                ( updatedWorld, cmd ) =
                    World.update worldMsg model.world

                mainCmd =
                    Cmd.map WorldMsg cmd
            in
            ( { model | world = updatedWorld }, mainCmd )

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

        PendingSeed text ->
            ( { model | pendingSeed = String.toInt text }, Cmd.none )

        UserUpdateSeed ->
            case model.pendingSeed of
                Just seed ->
                    update (UpdateSeed seed) model

                Nothing ->
                    ( model, Cmd.none )

        Regenerate ->
            ( { model | pendingSeed = Nothing }, Random.generate UpdateSeed World.noiseSeedGenerator )

        UpdateSeed noiseSeed ->
            let
                world =
                    model.world

                updatedWorld =
                    World.updateSeed world noiseSeed

                updatedGame =
                    { model | world = updatedWorld }
            in
            ( updatedGame, Cmd.none )



--- VIEW


view : Model -> Browser.Document Msg
view game =
    let
        body =
            [ Html.div [ style "text-align" "center" ]
                [ World.view game.world game.worldCamera |> Html.map WorldMsg
                , editorControls game
                ]
            ]
    in
    { title = "Island Wars"
    , body = body
    }


editorControls : Model -> Html Msg
editorControls game =
    Html.div [ Html.Attributes.class "editor-controls" ]
        [ editorInputs game
        , noiseInputs game
        , advancedInputs game
        , changeIslandInputs game
        ]


changeIslandInputs : Model -> Html Msg
changeIslandInputs model =
    let
        seed =
            model.pendingSeed
                |> Maybe.map String.fromInt
                |> Maybe.withDefault ""
    in
    Html.div [ Html.Attributes.class "change-island-inputs" ]
        [ Html.div [] [ text ("Current Island: " ++ String.fromInt model.world.noiseSeed) ]
        , Html.div [] [ Html.button [ Html.Events.onClick Regenerate ] [ text "Random Island" ] ]
        , Html.div [ Html.Attributes.class "inline-input" ]
            [ Html.label [ Html.Attributes.for "pending-seed" ] [ text "Island #" ]
            , Html.input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.id "pending-seed"
                , Html.Attributes.value seed
                , Html.Events.onInput PendingSeed
                ]
                []
            , Html.button [ Html.Events.onClick UserUpdateSeed ] [ text "Go!" ]
            ]
        ]


advancedInputs : Model -> Html Msg
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
                , Html.Attributes.step "0.1"
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


noiseInputs : Model -> Html Msg
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


editorInputs : Model -> Html Msg
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
