module DictHelper exposing (any, updateWhere)

import Dict exposing (..)


updateWhere : (comparable -> v -> Bool) -> (comparable -> v -> v) -> Dict comparable v -> Dict comparable v
updateWhere whereClause updateFn dict =
    dict
        |> Dict.filter whereClause
        |> Dict.map updateFn
        |> (\d -> Dict.union d dict)


any : (comparable -> v -> Bool) -> Dict comparable v -> Bool
any predicate dict =
    dict
        |> Dict.toList
        |> List.any (\t -> predicate (Tuple.first t) (Tuple.second t))
