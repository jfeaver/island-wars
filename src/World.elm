module World exposing (..)

import Camera exposing (Camera2D)
import Canvas exposing (rect, shapes)
import Canvas.Settings exposing (fill)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events.Extra.Mouse exposing (Button(..), Event, onClick, onMove)
import Island exposing (Island)
import Island.IslandType exposing (IslandType(..))
import Island.LandType
import Island.View
import Point
import Random
import Simplex


{-| world units
-}
hexSize : Float
hexSize =
    8


init : Seed -> World
init worldSeed =
    let
        singleIsland =
            Island.init
                { center = ( 37, 13 )
                , size = 70
                , iType = Vanilla
                }
    in
    { permutationTable = worldSeed.permutationTable
    , island = singleIsland
    , islands =
        [ singleIsland
        ]
    , godsHand = False
    , activeIsland = Just singleIsland
    , focus = 3
    }


type alias World =
    { permutationTable : Simplex.PermutationTable
    , island : Island
    , islands : List Island
    , godsHand : Bool -- Is the cursor over some clickable entity?
    , activeIsland : Maybe Island
    , focus : Float
    }


type alias Seed =
    { size : Int
    , iType : IslandType
    , permutationTable : Simplex.PermutationTable
    }


type Msg
    = MouseMove Event
    | MouseClick Event


godsHandOverIsland : World -> Canvas.Point -> Maybe Island
godsHandOverIsland _ _ =
    -- List.Extra.find (Island.Collision.insideMaxRadius hexSize point) islands
    Nothing


godsHandOverAnyIsland : World -> Canvas.Point -> Bool
godsHandOverAnyIsland _ _ =
    -- List.any (Island.Collision.insideMaxRadius hexSize point) islands
    False


update : Msg -> World -> ( World, Cmd Msg )
update msg world =
    case msg of
        MouseMove { offsetPos } ->
            if godsHandOverAnyIsland world offsetPos then
                ( { world | godsHand = True }, Cmd.none )

            else
                ( { world | godsHand = False }, Cmd.none )

        MouseClick { button, offsetPos } ->
            if button == MainButton then
                let
                    mIsland =
                        godsHandOverIsland world offsetPos
                in
                ( { world | activeIsland = mIsland }, Cmd.none )

            else
                ( world, Cmd.none )


ocean : Camera2D -> Canvas.Renderable
ocean camera =
    let
        ( width, height ) =
            camera
                |> Camera.getViewport
                |> Point.fromCoordinate
    in
    shapes [ fill <| Island.LandType.toColor Island.LandType.Ocean ] [ rect ( 0, 0 ) width height ]


view : World -> Camera2D -> Html Msg
view world camera =
    let
        islandRenderConfig : Island.View.RenderableConfig
        islandRenderConfig =
            { camera = camera
            , worldHexSize = hexSize
            , focus = world.focus
            , permutationTable = world.permutationTable
            , noiseConfig =
                { steps = 3
                , stepSize = 2.0
                , persistence = 2.0
                , scale = hexSize * 2
                }
            }

        islands =
            world.islands
                |> List.map (Island.View.renderable islandRenderConfig)
                |> Canvas.group []

        renderables =
            [ ocean camera
            , islands

            -- , activeIslandIndicator world camera
            ]

        cursor =
            if world.godsHand then
                "pointer"

            else
                "auto"
    in
    Canvas.toHtml (Camera.getViewport camera) [ onMove MouseMove, onClick MouseClick, style "cursor" cursor ] renderables



-- activeIslandIndicator : World -> Camera2D -> Canvas.Renderable
-- activeIslandIndicator { activeIsland } camera =
--     case activeIsland of
--         Just island ->
--             Camera.islandIndicator camera island
--         Nothing ->
--             Canvas.group [] []
-- activeIslandIndicator : World -> Canvas.Renderable
-- activeIslandIndicator { activeIsland } =
--     case activeIsland of
--         Just island ->
--             let
--                 center =
--                     GridPoint.toCanvas hexSize island.center
--                 radius =
--                     Island.maxRadius island.size |> toFloat |> GridPoint.canvasDistance hexSize |> (+) (5 * hexSize)
--             in
--             shapes [ stroke Color.white, lineWidth ((hexSize + 5) / 3), lineDash [ 5 * hexSize, 3 * hexSize ] ] [ circle center radius ]
--         Nothing ->
--             Canvas.group [] []


seed : Random.Generator Seed
seed =
    Random.map3 Seed
        Island.randomSizeGenerator
        Island.randomTypeGenerator
        Simplex.permutationTableGenerator
