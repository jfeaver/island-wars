module Main exposing (..)

import Browser
import Html exposing (button, div, text)
import Html.Events exposing (onClick)


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
    | InGame


init : () -> ( Model, Cmd Msg )
init =
    always
        ( MainMenu
        , Cmd.none
        )



--- UPDATE


type Msg
    = NoOp
    | StartGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartGame ->
            ( InGame, Cmd.none )



--- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        MainMenu ->
            { title = "Canvas Test"
            , body =
                [ div [] [ button [ onClick StartGame ] [ text "Hello Menu" ] ]
                ]
            }

        InGame ->
            Debug.todo "branch 'InGame' not implemented"
