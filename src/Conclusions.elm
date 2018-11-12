module Conclusions exposing
    ( HoldingStatus(..)
    , getHoldingStatus
    )

import Clue exposing (CardId, CompleteGuess)
import Dict exposing (..)
import DictHelper
import Player exposing (Player, PlayerId, Players)


type HoldingStatus
    = NotHolding
    | MaybeHolding Int
    | Holding


getHoldingStatus : Players -> List CompleteGuess -> CardId -> PlayerId -> Maybe HoldingStatus
getHoldingStatus players guessHistory cardId playerId =
    setupConclusions (Player.allIds players)
        |> applyHistoricalFacts guessHistory
        |> analyze players guessHistory
        |> Dict.get ( cardId, playerId )


type alias Conclusions =
    Dict ( CardId, PlayerId ) HoldingStatus


setupConclusions : List PlayerId -> Conclusions
setupConclusions playerIds =
    let
        setInitialConclusions =
            \card playerId conclusions ->
                Dict.insert ( Clue.getCardId card, playerId ) (MaybeHolding 0) conclusions

        reducer =
            \card conclusions -> List.foldl (setInitialConclusions card) conclusions playerIds
    in
    List.foldl reducer Dict.empty Clue.allCards


applyHistoricalFacts : List CompleteGuess -> Conclusions -> Conclusions
applyHistoricalFacts history conclusions =
    List.foldl applyGuessFacts conclusions history


applyGuessFacts : CompleteGuess -> Conclusions -> Conclusions
applyGuessFacts guess conclusions =
    conclusions
        |> setNoShowStatuses guess
        |> setShowerStatuses guess


setNoShowStatuses : CompleteGuess -> Conclusions -> Conclusions
setNoShowStatuses guess conclusions =
    List.foldl (setNoShowToNotHaveCards guess) conclusions (Clue.getNoShows guess)


setNoShowToNotHaveCards : CompleteGuess -> PlayerId -> Conclusions -> Conclusions
setNoShowToNotHaveCards guess playerId conclusions =
    let
        reducer =
            \cardId cs -> Dict.insert ( cardId, playerId ) NotHolding cs
    in
    List.foldl reducer conclusions (Clue.getCardIdsFromGuess guess)


setShowerStatuses : CompleteGuess -> Conclusions -> Conclusions
setShowerStatuses guess conclusions =
    let
        reducer =
            \showerId cardId cs -> Dict.update ( cardId, showerId ) incrementMaybe cs

        maybeSetter =
            \showerId -> List.foldl (reducer showerId) conclusions (Clue.getCardIdsFromGuess guess)

        revealedSetter =
            \showerId revealedCardId cs -> Dict.insert ( revealedCardId, showerId ) Holding cs
    in
    case Clue.getShower guess of
        Just ( showerId, Just revealedCardId ) ->
            maybeSetter showerId
                |> revealedSetter showerId revealedCardId

        Just ( showerId, Nothing ) ->
            maybeSetter showerId

        Nothing ->
            conclusions


incrementMaybe : Maybe HoldingStatus -> Maybe HoldingStatus
incrementMaybe status =
    case status of
        Just (MaybeHolding count) ->
            Just (MaybeHolding (count + 1))

        _ ->
            status


analyze : Players -> List CompleteGuess -> Conclusions -> Conclusions
analyze players history conclusions =
    let
        newConclusions =
            conclusions
                |> noPlayersCanHoldAnyCardSomeoneElseIsHolding
                |> playersWhoShowedOneOfThreeCardsInThePastMustHoldOneOfThoseCards players history
                |> playersCannotExceedMaximumNumberOfCards players
    in
    if newConclusions == conclusions then
        newConclusions

    else
        analyze players history newConclusions


noPlayerCanHoldACardSomeoneElseIsHolding : CardId -> Conclusions -> Conclusions
noPlayerCanHoldACardSomeoneElseIsHolding cardId conclusions =
    let
        conclusionsForThisCard =
            Dict.filter (\conclusionKey _ -> cardId == Tuple.first conclusionKey) conclusions
    in
    if DictHelper.any (\_ v -> v == Holding) conclusionsForThisCard then
        conclusionsForThisCard
            |> DictHelper.updateWhere (\_ v -> v /= Holding) (\_ _ -> NotHolding)
            |> (\d -> Dict.union d conclusions)

    else
        conclusions


noPlayersCanHoldAnyCardSomeoneElseIsHolding : Conclusions -> Conclusions
noPlayersCanHoldAnyCardSomeoneElseIsHolding conclusions =
    let
        cardIds =
            List.map Clue.getCardId Clue.allCards
    in
    List.foldl noPlayerCanHoldACardSomeoneElseIsHolding conclusions cardIds


checkCombination : PlayerId -> List CardId -> Conclusions -> Conclusions
checkCombination playerId cardIds conclusions =
    let
        factsForPlayer =
            conclusions
                |> Dict.filter (\key _ -> Tuple.second key == playerId)

        numberFromComboNotHolding =
            factsForPlayer
                |> Dict.filter (\key _ -> List.member (Tuple.first key) cardIds)
                |> Dict.filter (\_ status -> status == NotHolding)
                |> Dict.size
    in
    if numberFromComboNotHolding == 2 then
        factsForPlayer
            |> DictHelper.updateWhere (\_ v -> v /= NotHolding) (\_ _ -> Holding)
            |> (\d -> Dict.union d conclusions)

    else
        conclusions


playerShowedCardForGuess : PlayerId -> CompleteGuess -> Bool
playerShowedCardForGuess playerId guess =
    case Clue.getShower guess of
        Just ( showerId, _ ) ->
            showerId == playerId

        Nothing ->
            False


checkPlayerHistory : List CompleteGuess -> PlayerId -> Conclusions -> Conclusions
checkPlayerHistory history playerId conclusions =
    let
        playerRevealCombinations =
            history
                |> List.filter (playerShowedCardForGuess playerId)
                |> List.map Clue.getCardIdsFromGuess
    in
    List.foldl (checkCombination playerId) conclusions playerRevealCombinations


playersWhoShowedOneOfThreeCardsInThePastMustHoldOneOfThoseCards : Players -> List CompleteGuess -> Conclusions -> Conclusions
playersWhoShowedOneOfThreeCardsInThePastMustHoldOneOfThoseCards players history conclusions =
    List.foldl (checkPlayerHistory history) conclusions (Player.allIds players)


playersCannotExceedMaximumNumberOfCards : Players -> Conclusions -> Conclusions
playersCannotExceedMaximumNumberOfCards players conclusions =
    let
        numberOfPlayers =
            List.length (Player.allIds players)

        numberOfPlayersCanDetermineMaxCards =
            numberOfPlayers == 3 || numberOfPlayers == 6
    in
    if numberOfPlayersCanDetermineMaxCards then
        let
            maxCards =
                if List.length (Player.allIds players) == 3 then
                    4

                else
                    3
        in
        List.foldl (ensureNoPlayerHasMoreThanNCards maxCards) conclusions (Player.allPlayers players)

    else
        conclusions


ensureNoPlayerHasMoreThanNCards : Int -> Player -> Conclusions -> Conclusions
ensureNoPlayerHasMoreThanNCards maxCards player conclusions =
    let
        playerId =
            Player.getId player

        conclusionsForPlayer =
            Dict.filter (\conclusionKey _ -> Tuple.second conclusionKey == playerId) conclusions

        numberOfCardsKnownToBeHeldByPlayer =
            conclusions
                |> Dict.filter (\conclusionKey holdingStatus -> Tuple.second conclusionKey == playerId && holdingStatus == Holding)
                |> Dict.size
    in
    if numberOfCardsKnownToBeHeldByPlayer == maxCards then
        conclusionsForPlayer
            |> DictHelper.updateWhere (\_ v -> v /= Holding) (\_ _ -> NotHolding)
            |> (\d -> Dict.union d conclusions)

    else
        conclusions
