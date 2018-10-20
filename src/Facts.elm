module Facts exposing
    ( Facts
    , HoldingStatus(..)
    , analyze
    , getHoldingStatus
    , initFacts
    , openingFacts
    , setPlayerDoesNotHaveCard
    , setPlayerHasCard
    , setPlayerMightHaveCard
    )

import Dict exposing (Dict, empty, get, insert, update)
import Domain exposing (Card(..), CompleteGuess, Person, PlayerId, Room, Weapon)


type HoldingStatus
    = NotHolding
    | MaybeHolding Int
    | Holding


type alias Facts =
    Dict ( String, PlayerId ) HoldingStatus


initFacts : Facts
initFacts =
    Dict.empty


setInitialFacts : Card -> PlayerId -> Facts -> Facts
setInitialFacts card playerId facts =
    Dict.insert ( keyForCard card, playerId ) (MaybeHolding 0) facts


openingFacts : List Card -> List PlayerId -> Facts
openingFacts cards players =
    let
        reducer =
            \card facts -> List.foldl (setInitialFacts card) facts players
    in
    List.foldl reducer Dict.empty cards


analyze : List CompleteGuess -> Facts -> Facts
analyze history facts =
    let
        newFacts =
            facts
    in
    if newFacts == facts then
        newFacts

    else
        analyze history newFacts


setPlayerMightHaveCard : Card -> PlayerId -> Facts -> Facts
setPlayerMightHaveCard card playerId facts =
    Dict.update ( keyForCard card, playerId ) incrementMaybe facts


incrementMaybe : Maybe HoldingStatus -> Maybe HoldingStatus
incrementMaybe status =
    case status of
        Just (MaybeHolding count) ->
            Just (MaybeHolding (count + 1))

        _ ->
            status


setPlayerHasCard : Card -> PlayerId -> Facts -> Facts
setPlayerHasCard card playerId facts =
    Dict.update ( keyForCard card, playerId ) (\_ -> Just Holding) facts


setPlayerDoesNotHaveCard : Card -> PlayerId -> Facts -> Facts
setPlayerDoesNotHaveCard card playerId facts =
    Dict.insert ( keyForCard card, playerId ) NotHolding facts


getHoldingStatus : Facts -> Card -> PlayerId -> Maybe HoldingStatus
getHoldingStatus facts card playerId =
    Dict.get ( keyForCard card, playerId ) facts


keyForPerson : Person -> String
keyForPerson person =
    Debug.toString person


keyForWeapon : Weapon -> String
keyForWeapon weapon =
    Debug.toString weapon


keyForRoom : Room -> String
keyForRoom room =
    Debug.toString room


keyForCard : Card -> String
keyForCard card =
    case card of
        PersonTag person ->
            keyForPerson person

        WeaponTag weapon ->
            keyForWeapon weapon

        RoomTag room ->
            keyForRoom room
