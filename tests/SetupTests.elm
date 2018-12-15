module SetupTests exposing (all)

import Expect
import Player exposing (Players)
import Setup exposing (..)
import Test exposing (..)


extractPlayers : Result a Players -> Players
extractPlayers result =
    case result of
        Ok players ->
            players

        Err _ ->
            Debug.todo "Expected setup to be complete and players available"


extractIncompleteSetup : Result Setup a -> Setup
extractIncompleteSetup result =
    case result of
        Ok _ ->
            Debug.todo "Expected setup to be incomplete, but was finished with players available"

        Err setup ->
            setup


addPlayerHelper : String -> Setup -> Setup
addPlayerHelper name setup =
    setup |> Setup.buildPlayerName name |> Setup.addPlayer


all : Test
all =
    describe "Setup"
        [ test """
        When three players are added,
        setup can be completed
        and each player has 6 cards.
        """ <|
            \_ ->
                let
                    players =
                        beginSetup
                            |> addPlayerHelper "Zach"
                            |> addPlayerHelper "Allison"
                            |> addPlayerHelper "Bill"
                            |> completeSetup
                            |> extractPlayers

                    expectedCardCounts =
                        [ 6, 6, 6 ]

                    actualCardCounts =
                        List.map Player.getNumberOfCardsInHand (Player.allPlayers players)
                in
                Expect.equal expectedCardCounts actualCardCounts
        , test """
        When six players are added,
        setup can be completed
        and each player has 3 cards.
        """ <|
            \_ ->
                let
                    players =
                        beginSetup
                            |> addPlayerHelper "Zach"
                            |> addPlayerHelper "Allison"
                            |> addPlayerHelper "Bill"
                            |> addPlayerHelper "Clare"
                            |> addPlayerHelper "Dan"
                            |> addPlayerHelper "Ellie"
                            |> completeSetup
                            |> extractPlayers

                    expectedCardCounts =
                        [ 3, 3, 3, 3, 3, 3 ]

                    actualCardCounts =
                        List.map Player.getNumberOfCardsInHand (Player.allPlayers players)
                in
                Expect.equal expectedCardCounts actualCardCounts
        , test """
        When four players are added,
        setup is not complete until two players are marked as having fewer cards,
        and those two players have 4 cards while the other two have 5 cards.
        """ <|
            \_ ->
                let
                    players =
                        beginSetup
                            |> addPlayerHelper "Zach"
                            |> addPlayerHelper "Allison"
                            |> addPlayerHelper "Bill"
                            |> addPlayerHelper "Clare"
                            |> completeSetup
                            |> extractIncompleteSetup
                            |> setDisadvantagedPlayer "Zach"
                            |> completeSetup
                            |> extractIncompleteSetup
                            |> setDisadvantagedPlayer "Clare"
                            |> completeSetup
                            |> extractPlayers

                    expectedCardCounts =
                        [ 4, 5, 5, 4 ]

                    actualCardCounts =
                        List.map Player.getNumberOfCardsInHand (Player.allPlayers players)
                in
                Expect.equal expectedCardCounts actualCardCounts
        , test """
        When five players are added,
        setup is not complete until two players are marked as having fewer cards,
        and those two players have 3 cards while the other three have 4 cards.
        """ <|
            \_ ->
                let
                    players =
                        beginSetup
                            |> addPlayerHelper "Zach"
                            |> addPlayerHelper "Allison"
                            |> addPlayerHelper "Bill"
                            |> addPlayerHelper "Clare"
                            |> addPlayerHelper "Dan"
                            |> completeSetup
                            |> extractIncompleteSetup
                            |> setDisadvantagedPlayer "Zach"
                            |> completeSetup
                            |> extractIncompleteSetup
                            |> setDisadvantagedPlayer "Clare"
                            |> completeSetup
                            |> extractPlayers

                    expectedCardCounts =
                        [ 3, 4, 4, 3, 4 ]

                    actualCardCounts =
                        List.map Player.getNumberOfCardsInHand (Player.allPlayers players)
                in
                Expect.equal expectedCardCounts actualCardCounts
        ]
