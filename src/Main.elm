module Main exposing (..)

import Browser
import Canvas.Settings.Text exposing (TextAlign(..))
import Html exposing (button, div, text)
import Html.Events exposing (onClick)
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
    | InGame World


init : () -> ( Model, Cmd Msg )
init =
    always <| update StartGame MainMenu



--- UPDATE


type Msg
    = StartGame
    | SeedWorld World.Seed
    | WorldMsg World.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( model, Random.generate SeedWorld World.seed )

        SeedWorld worldSeed ->
            ( InGame (World.init worldSeed), Cmd.none )

        WorldMsg worldMsg ->
            case model of
                InGame world ->
                    let
                        ( updatedWorld, cmd ) =
                            World.update worldMsg world

                        mainCmd =
                            Cmd.map WorldMsg cmd
                    in
                    ( InGame updatedWorld, mainCmd )

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

                InGame world ->
                    [ World.view world |> Html.map WorldMsg
                    ]
    in
    { title = "Island Wars"
    , body = body
    }
