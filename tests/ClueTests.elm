module ClueTests exposing (all)

import Clue exposing (..)
import Expect
import Test exposing (..)


isComplete : Result a CompleteGuess -> Bool
isComplete guessResult =
    case guessResult of
        Ok _ ->
            True

        _ ->
            False


extractGuess : Result a CompleteGuess -> CompleteGuess
extractGuess result =
    case result of
        Ok completeGuess ->
            completeGuess

        Err _ ->
            Debug.todo "Guess created in test was expected to be complete but was not"


all : Test
all =
    describe "Clue"
        [ describe "guessing"
            [ test "a guess is initally constructed with a player, but is not yet complete" <|
                \_ ->
                    let
                        guesserId =
                            5
                    in
                    Expect.false "Expected guess to be incomplete" (beginGuess guesserId |> finishGuess |> isComplete)
            , test "a person (or weapon or room) card can be added to a guess, but the guess is still incomplete" <|
                \_ ->
                    let
                        guesserId =
                            5

                        guess =
                            beginGuess guesserId
                                |> addCardToGuess "plum"
                                |> finishGuess
                    in
                    Expect.false "Expected guess to be incomplete" (isComplete guess)
            , test "the three cards must be one person, one weapon, and one room" <|
                \_ ->
                    let
                        guesserId =
                            5

                        guess =
                            beginGuess guesserId
                                |> addCardToGuess "plum"
                                |> addCardToGuess "mustard"
                                |> addCardToGuess "knife"
                                |> finishGuess
                    in
                    Expect.false "Expected guess to be incomplete" (isComplete guess)
            , test "a guess with person, weapon, and room all set is complete" <|
                \_ ->
                    let
                        guesserId =
                            5

                        guess =
                            beginGuess guesserId
                                |> addCardToGuess "plum"
                                |> addCardToGuess "rope"
                                |> addCardToGuess "hall"
                                |> finishGuess
                    in
                    Expect.true "Expected guess to be complete" (isComplete guess)
            , test "adding noShows to a guess" <|
                \_ ->
                    let
                        guesserId =
                            5

                        noShowIdOne =
                            1

                        noShowIdTwo =
                            2

                        guess =
                            beginGuess guesserId
                                |> addCardToGuess "plum"
                                |> addCardToGuess "rope"
                                |> addCardToGuess "hall"
                                |> finishGuess
                                |> extractGuess
                                |> addNoShowToGuess noShowIdOne
                                |> addNoShowToGuess noShowIdTwo
                    in
                    Expect.equal [ noShowIdTwo, noShowIdOne ] (getNoShows guess)
            , test "adding a shower to a guess" <|
                \_ ->
                    let
                        guesserId =
                            5

                        showerId =
                            3

                        guess =
                            beginGuess guesserId
                                |> addCardToGuess "plum"
                                |> addCardToGuess "rope"
                                |> addCardToGuess "hall"
                                |> finishGuess
                                |> extractGuess
                                |> addShowerToGuess showerId
                    in
                    Expect.equal (Just showerId) (getShower guess)
            , test "retrieving the card IDs for the person, weapon, and room from a complete guess" <|
                \_ ->
                    let
                        guesserId =
                            5

                        guess =
                            beginGuess guesserId
                                |> addCardToGuess "plum"
                                |> addCardToGuess "rope"
                                |> addCardToGuess "hall"
                                |> finishGuess
                                |> extractGuess

                        expectedIds =
                            [ "plum", "rope", "hall" ]
                    in
                    Expect.equal expectedIds (getCardIdsFromGuess guess)
            ]
        ]
