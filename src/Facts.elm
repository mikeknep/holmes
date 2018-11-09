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
import DictHelper exposing (..)
import Domain exposing (Card(..), CompleteGuess, Person, Room, Weapon, allCards)
import Player exposing (PlayerId)


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


noPlayerCanHoldACardSomeoneElseIsHolding : Card -> Facts -> Facts
noPlayerCanHoldACardSomeoneElseIsHolding card facts =
    let
        factsForThisCard =
            Dict.filter (\factKey _ -> keyForCard card == Tuple.first factKey) facts
    in
    if any (\_ v -> v == Holding) factsForThisCard then
        factsForThisCard
            |> updateWhere (\_ v -> v /= Holding) (\_ _ -> NotHolding)
            |> (\d -> Dict.union d facts)

    else
        facts


noPlayersCanHoldAnyCardSomeoneElseIsHolding : Facts -> Facts
noPlayersCanHoldAnyCardSomeoneElseIsHolding facts =
    List.foldr noPlayerCanHoldACardSomeoneElseIsHolding facts allCards


playerCountCanDetermineMaxCards : Int -> Bool
playerCountCanDetermineMaxCards playerCount =
    playerCount == 3 || playerCount == 6


getMaxCardsPerPlayer : Int -> Int
getMaxCardsPerPlayer playerCount =
    if playerCount == 3 then
        4

    else
        3


restrictToNCards : Int -> Int -> Facts -> Facts
restrictToNCards maxCards playerId facts =
    let
        factsForPlayer =
            Dict.filter (\factKey _ -> Tuple.second factKey == playerId) facts

        numberOfCardsHeldByPlayer =
            facts
                |> Dict.filter (\factKey holdingStatus -> Tuple.second factKey == playerId && holdingStatus == Holding)
                |> Dict.size
    in
    if numberOfCardsHeldByPlayer == maxCards then
        factsForPlayer
            |> updateWhere (\_ v -> v /= Holding) (\_ _ -> NotHolding)
            |> (\d -> Dict.union d facts)

    else
        facts


ensureNoPlayerHasMoreThanNCards : List Int -> Int -> Facts -> Facts
ensureNoPlayerHasMoreThanNCards playerIds maxCards facts =
    List.foldr (restrictToNCards maxCards) facts playerIds


playersCannotExceedMaximumNumberOfCards : List Int -> Facts -> Facts
playersCannotExceedMaximumNumberOfCards playerIds facts =
    if playerCountCanDetermineMaxCards (List.length playerIds) then
        let
            maxCardsPerPlayer =
                getMaxCardsPerPlayer (List.length playerIds)
        in
        ensureNoPlayerHasMoreThanNCards playerIds maxCardsPerPlayer facts

    else
        facts


checkCombination : PlayerId -> List Card -> Facts -> Facts
checkCombination playerId cards facts =
    let
        factsForPlayer =
            facts
                |> Dict.filter (\key _ -> Tuple.second key == playerId)

        numberFromComboNotHolding =
            factsForPlayer
                |> Dict.filter (\key _ -> List.member (Tuple.first key) (List.map keyForCard cards))
                |> Dict.filter (\_ status -> status == NotHolding)
                |> Dict.size
    in
    if numberFromComboNotHolding == 2 then
        factsForPlayer
            |> updateWhere (\_ v -> v /= NotHolding) (\_ _ -> Holding)
            |> (\d -> Dict.union d facts)

    else
        facts


checkPlayerHistory : List CompleteGuess -> PlayerId -> Facts -> Facts
checkPlayerHistory history playerId facts =
    let
        playerRevealCombinations =
            history
                |> List.filter (\guess -> guess.shower == Just playerId)
                |> List.map (\guess -> [ PersonTag guess.person, WeaponTag guess.weapon, RoomTag guess.room ])
    in
    List.foldr (checkCombination playerId) facts playerRevealCombinations


playersWhoShowedOneOfThreeCardsInThePastMustHoldOneOfThoseCards : List CompleteGuess -> List PlayerId -> Facts -> Facts
playersWhoShowedOneOfThreeCardsInThePastMustHoldOneOfThoseCards history playerIds facts =
    List.foldr (checkPlayerHistory history) facts playerIds


analyze : List Int -> List CompleteGuess -> Facts -> Facts
analyze playerIds history facts =
    let
        newFacts =
            facts
                |> noPlayersCanHoldAnyCardSomeoneElseIsHolding
                |> playersWhoShowedOneOfThreeCardsInThePastMustHoldOneOfThoseCards history playerIds
                |> playersCannotExceedMaximumNumberOfCards playerIds
    in
    if newFacts == facts then
        newFacts

    else
        analyze playerIds history newFacts


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
