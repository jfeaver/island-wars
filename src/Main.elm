module Main exposing (..)

import Browser
import Canvas exposing (rect, shapes)
import Canvas.Settings exposing (fill)
import Color
import Html exposing (Html, button, canvas, div, text)
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
    let
        body =
            case model of
                MainMenu ->
                    [ div [] [ button [ onClick StartGame ] [ text "New Game" ] ]
                    ]

                InGame ->
                    [ div [] [ text "Hello Game" ]
                    , canvas model
                    ]
    in
    { title = "Island Wars"
    , body = body
    }


canvas : Model -> Html Msg
canvas model =
    let
        renderables =
            [ shapes [ fill Color.green ] [ rect ( 100, 100 ) 50 100 ] ]
    in
    Canvas.toHtml ( 500, 500 ) [] renderables
