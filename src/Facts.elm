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
import Domain exposing (Card(..), CompleteGuess, Person, Player, Room, Weapon)


type HoldingStatus
    = NotHolding
    | MaybeHolding Int
    | Holding


type alias Facts =
    Dict ( String, String ) HoldingStatus


initFacts : Facts
initFacts =
    Dict.empty


setInitialFacts : Card -> Player -> Facts -> Facts
setInitialFacts card player facts =
    Dict.insert ( keyForCard card, keyForPlayer player ) (MaybeHolding 0) facts


openingFacts : List Card -> List Player -> Facts
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


setPlayerMightHaveCard : Card -> Player -> Facts -> Facts
setPlayerMightHaveCard card player facts =
    Dict.update ( keyForCard card, keyForPlayer player ) incrementMaybe facts


incrementMaybe : Maybe HoldingStatus -> Maybe HoldingStatus
incrementMaybe status =
    case status of
        Just (MaybeHolding count) ->
            Just (MaybeHolding (count + 1))

        _ ->
            status


setPlayerHasCard : Card -> Player -> Facts -> Facts
setPlayerHasCard card player facts =
    Dict.update ( keyForCard card, keyForPlayer player ) (\_ -> Just Holding) facts


setPlayerDoesNotHaveCard : Card -> Player -> Facts -> Facts
setPlayerDoesNotHaveCard card player facts =
    Dict.insert ( keyForCard card, keyForPlayer player ) NotHolding facts


getHoldingStatus : Facts -> Card -> Player -> Maybe HoldingStatus
getHoldingStatus facts card player =
    Dict.get ( keyForCard card, keyForPlayer player ) facts


keyForPlayer : Player -> String
keyForPlayer player =
    player.name


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
