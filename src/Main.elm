module Main exposing (..)

import Browser
import Camera exposing (Camera2D)
import Canvas.Settings.Text exposing (TextAlign(..))
import Html exposing (button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Island.IslandType exposing (IslandType)
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
    -- always <| ( MainMenu, Cmd.none )
    always <| update StartGame MainMenu



-- TODO:
-- 1. Find an initial location for a settlement
-- 2. draw the settlement
-- 3. center/zoom the camera there
-- 4. command the settlement to go fishing?


cameraInit : Camera2D
cameraInit =
    Camera.init ( 1400, 700 )


gameInit : World.Seed -> Game
gameInit worldSeed =
    { world = World.init worldSeed
    , worldCamera = cameraInit
    }


staticWorld : Game
staticWorld =
    { world = World.static
    , worldCamera = cameraInit
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
            -- ( model, Random.generate SeedWorld World.seed )
            ( InGame staticWorld, Cmd.none )

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
                        ]
                    ]
    in
    { title = "Island Wars"
    , body = body
    }
