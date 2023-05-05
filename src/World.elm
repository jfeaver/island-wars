module World exposing (..)

-- import Perlin

import Canvas exposing (rect, shapes)
import Canvas.Settings exposing (fill)
import Color
import Hexagon exposing (GridPoint, hexHorizSpacing, hexVertSpacing)
import Html exposing (Html)
import Island exposing (Island)
import Island.IslandType exposing (IslandType)
import Random
import Simplex


oceanColor =
    Color.rgb255 0 64 128


height =
    500


width =
    1000


hexSize =
    10


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
    }


type Msg
    = NoOp


type alias World =
    { islands : List Island
    }


{-| perlinGradients is a grid of radian vectors
-}
type alias Seed =
    { size : Int
    , iType : IslandType

    -- , perlinGradients : Perlin.Gradients2D
    , permutationTable : Simplex.PermutationTable
    }


view : World -> Html msg
view world =
    let
        islands =
            world.islands
                |> List.map (Island.renderable hexSize)
                |> Canvas.group []

        renderables =
            [ ocean
            , islands
            ]
    in
    Canvas.toHtml ( width, height ) [] renderables


ocean : Canvas.Renderable
ocean =
    shapes [ fill oceanColor ] [ rect ( 0, 0 ) width height ]


seed : Random.Generator Seed
seed =
    Random.map3 Seed
        Island.randomSizeGenerator
        Island.randomTypeGenerator
        -- (Perlin.gradients2dGenerator gridWidth gridHeight)
        Simplex.permutationTableGenerator
