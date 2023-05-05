module Island.IslandType exposing (..)


type IslandType
    = Vanilla
    | Woods
    | Cliffs
    | Mountains


toString : IslandType -> String
toString islandType =
    case islandType of
        Vanilla ->
            ""

        Woods ->
            "woods"

        Cliffs ->
            "cliffs"

        Mountains ->
            "mountains"


fromString : String -> Maybe IslandType
fromString string =
    case string of
        "" ->
            Just Vanilla

        "woods" ->
            Just Woods

        "cliffs" ->
            Just Cliffs

        "mountains" ->
            Just Mountains

        _ ->
            Nothing
