module Facts exposing
    ( Facts
    , HoldingStatus(..)
    , getHoldingStatus
    , initFacts
    , openingFacts
    , setPlayerDoesNotHaveCard
    , setPlayerHasCard
    , setPlayerMightHaveCard
    )

import Dict exposing (Dict, empty, get, insert, update)


type HoldingStatus
    = NotHolding
    | MaybeHolding Int
    | Holding


type alias Facts =
    Dict ( String, String ) HoldingStatus


initFacts : Facts
initFacts =
    Dict.empty


setInitialFacts : String -> String -> Facts -> Facts
setInitialFacts cardKey playerKey facts =
    Dict.insert ( cardKey, playerKey ) (MaybeHolding 0) facts


openingFacts : List String -> List String -> Facts
openingFacts cardKeys playerKeys =
    let
        reducer =
            \cardKey facts -> List.foldl (setInitialFacts cardKey) facts playerKeys
    in
    List.foldl reducer Dict.empty cardKeys


setPlayerMightHaveCard : String -> String -> Facts -> Facts
setPlayerMightHaveCard cardKey playerKey facts =
    Dict.update ( cardKey, playerKey ) incrementMaybe facts


incrementMaybe : Maybe HoldingStatus -> Maybe HoldingStatus
incrementMaybe status =
    case status of
        Just (MaybeHolding count) ->
            Just (MaybeHolding (count + 1))

        _ ->
            status


setPlayerHasCard : String -> String -> Facts -> Facts
setPlayerHasCard cardKey playerKey facts =
    Dict.update ( cardKey, playerKey ) (\_ -> Just Holding) facts


setPlayerDoesNotHaveCard : String -> String -> Facts -> Facts
setPlayerDoesNotHaveCard cardKey playerKey facts =
    Dict.insert ( cardKey, playerKey ) NotHolding facts


getHoldingStatus : Facts -> String -> String -> Maybe HoldingStatus
getHoldingStatus facts cardKey playerKey =
    Dict.get ( cardKey, playerKey ) facts
