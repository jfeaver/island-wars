module World exposing (..)

-- import Perlin

import Canvas exposing (circle, rect, shapes)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Line exposing (lineDash, lineWidth)
import Color
import GridPoint exposing (GridPoint, hexHorizSpacing, hexVertSpacing)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events.Extra.Mouse exposing (Button(..), Event, onClick, onMove)
import Island exposing (Island)
import Island.Collision
import Island.IslandType exposing (IslandType)
import List.Extra
import Random
import Simplex


oceanColor =
    Color.rgb255 0 64 128


height =
    500


width =
    1400


hexSize =
    1


gridHeight =
    ceiling (height / hexVertSpacing hexSize)


gridWidth =
    ceiling (width / hexHorizSpacing hexSize)


gridCenter : GridPoint
gridCenter =
    ( floor (toFloat gridWidth / 2), floor (toFloat gridHeight / 2) )


init : Seed -> World
init worldSeed =
    { islands =
        [ Island.init
            { center = gridCenter
            , size = worldSeed.size
            , iType = worldSeed.iType
            , permutationTable = worldSeed.permutationTable
            }
        ]
    , godsHand = False
    , activeIsland = Nothing
    }


type alias World =
    { islands : List Island
    , godsHand : Bool -- Is the cursor over some clickable entity?
    , activeIsland : Maybe Island
    }


{-| perlinGradients is a grid of radian vectors
-}
type alias Seed =
    { size : Int
    , iType : IslandType

    -- , perlinGradients : Perlin.Gradients2D
    , permutationTable : Simplex.PermutationTable
    }


type Msg
    = MouseMove Event
    | MouseClick Event


godsHandOverIsland : World -> Canvas.Point -> Maybe Island
godsHandOverIsland { islands } point =
    List.Extra.find (Island.Collision.insideMaxRadius hexSize point) islands


godsHandOverAnyIsland : World -> Canvas.Point -> Bool
godsHandOverAnyIsland { islands } point =
    List.any (Island.Collision.insideMaxRadius hexSize point) islands


update : Msg -> World -> ( World, Cmd Msg )
update msg world =
    case msg of
        MouseMove { offsetPos } ->
            if godsHandOverAnyIsland world offsetPos then
                ( { world | godsHand = True }, Cmd.none )

            else
                ( { world | godsHand = False }, Cmd.none )

        -- { button = MainButton, clientPos = (506,232), keys = { alt = False, ctrl = False, meta = False, shift = False }, offsetPos = (497,224), pagePos = (506,232), screenPos = (506,343) }
        MouseClick { button, offsetPos } ->
            if button == MainButton then
                let
                    mIsland =
                        godsHandOverIsland world offsetPos
                in
                ( { world | activeIsland = mIsland }, Cmd.none )

            else
                ( world, Cmd.none )


view : World -> Html Msg
view world =
    let
        islands =
            world.islands
                |> List.map (Island.renderable hexSize)
                |> Canvas.group []

        renderables =
            [ ocean
            , islands
            , activeIslandLocator world
            ]

        cursor =
            if world.godsHand then
                "pointer"

            else
                "auto"
    in
    Canvas.toHtml ( width, height ) [ onMove MouseMove, onClick MouseClick, style "cursor" cursor ] renderables


ocean : Canvas.Renderable
ocean =
    shapes [ fill oceanColor ] [ rect ( 0, 0 ) width height ]


activeIslandLocator : World -> Canvas.Renderable
activeIslandLocator { activeIsland } =
    case activeIsland of
        Just island ->
            let
                center =
                    GridPoint.toCanvas hexSize island.center

                radius =
                    Island.maxRadius island.size |> toFloat |> GridPoint.canvasDistance hexSize |> (+) (5 * hexSize)
            in
            shapes [ stroke Color.white, lineWidth ((hexSize + 5) / 3), lineDash [ 5 * hexSize, 3 * hexSize ] ] [ circle center radius ]

        Nothing ->
            Canvas.group [] []


seed : Random.Generator Seed
seed =
    Random.map3 Seed
        Island.randomSizeGenerator
        Island.randomTypeGenerator
        Simplex.permutationTableGenerator
