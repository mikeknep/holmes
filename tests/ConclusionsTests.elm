module ConclusionsTests exposing (all)

import Clue exposing (..)
import Conclusions exposing (..)
import Expect
import Player exposing (PlayerId, Players)
import Test exposing (..)


testPlayers : Players
testPlayers =
    Player.noPlayers
        |> Player.addNewPlayer "Zach"
        |> Player.addNewPlayer "Allison"
        |> Player.addNewPlayer "Bill"
        |> Player.addNewPlayer "Clare"
        |> Player.addNewPlayer "Dan"
        |> Player.addNewPlayer "Ellie"


extractGuess : Result a CompleteGuess -> CompleteGuess
extractGuess result =
    case result of
        Ok completeGuess ->
            completeGuess

        Err _ ->
            Debug.todo "Guess created in test was expected to be complete but was not"


all : Test
all =
    describe "Conclusions"
        [ test """
        When a player no-shows on a guess,
        that player is not holding any of the cards from that guess.
        """ <|
            \_ ->
                let
                    noShowId =
                        1

                    guess =
                        Clue.beginGuess 0
                            |> addCardToGuess (getCardId testPersonCard)
                            |> addCardToGuess (getCardId testWeaponCard)
                            |> addCardToGuess (getCardId testRoomCard)
                            |> finishGuess
                            |> extractGuess
                            |> addNoShowToGuess noShowId

                    expectedStatuses =
                        ( Just NotHolding
                        , Just NotHolding
                        , Just NotHolding
                        )

                    actualStatuses =
                        ( getHoldingStatus testPlayers [ guess ] (getCardId testPersonCard) noShowId
                        , getHoldingStatus testPlayers [ guess ] (getCardId testWeaponCard) noShowId
                        , getHoldingStatus testPlayers [ guess ] (getCardId testRoomCard) noShowId
                        )
                in
                Expect.equal expectedStatuses actualStatuses
        , test """
       When a player reveals some card from a guess,
       that player may be holding any of those cards.
       """ <|
            \_ ->
                let
                    showerId =
                        1

                    guess =
                        Clue.beginGuess 0
                            |> addCardToGuess (getCardId testPersonCard)
                            |> addCardToGuess (getCardId testWeaponCard)
                            |> addCardToGuess (getCardId testRoomCard)
                            |> finishGuess
                            |> extractGuess
                            |> addShowerToGuess showerId

                    expectedStatuses =
                        ( Just (MaybeHolding 1)
                        , Just (MaybeHolding 1)
                        , Just (MaybeHolding 1)
                        )

                    actualStatuses =
                        ( getHoldingStatus testPlayers [ guess ] (getCardId testPersonCard) showerId
                        , getHoldingStatus testPlayers [ guess ] (getCardId testWeaponCard) showerId
                        , getHoldingStatus testPlayers [ guess ] (getCardId testRoomCard) showerId
                        )
                in
                Expect.equal expectedStatuses actualStatuses
        , test """
        When a player shows some card for multiple guesses,
        and the guesses overlap on at least one card,
        the number of times the player has maybe shown each card is known.
        """ <|
            \_ ->
                let
                    showerId =
                        1

                    guessOne =
                        Clue.beginGuess 0
                            |> addCardToGuess (getCardId testPersonCard)
                            |> addCardToGuess (getCardId testWeaponCard)
                            |> addCardToGuess (getCardId testRoomCard)
                            |> finishGuess
                            |> extractGuess
                            |> addShowerToGuess showerId

                    guessTwo =
                        Clue.beginGuess 0
                            |> addCardToGuess (getCardId testPersonCard)
                            |> addCardToGuess (getCardId testWeaponCard)
                            |> addCardToGuess (getCardId testRoomCard)
                            |> finishGuess
                            |> extractGuess
                            |> addShowerToGuess showerId

                    expectedStatuses =
                        ( Just (MaybeHolding 2)
                        , Just (MaybeHolding 2)
                        , Just (MaybeHolding 2)
                        )

                    actualStatuses =
                        ( getHoldingStatus testPlayers [ guessOne, guessTwo ] (getCardId testPersonCard) showerId
                        , getHoldingStatus testPlayers [ guessOne, guessTwo ] (getCardId testWeaponCard) showerId
                        , getHoldingStatus testPlayers [ guessOne, guessTwo ] (getCardId testRoomCard) showerId
                        )
                in
                Expect.equal expectedStatuses actualStatuses
        , test """
        When a player reveals a specific card for a guess,
        that player is definitely holding the revealed card.
        That player may also be holding the other two cards from the guess.
        No other player is holding the revealed cards.
        """ <|
            \_ ->
                let
                    showerId =
                        1

                    otherPlayerId =
                        2

                    guess =
                        Clue.beginGuess 0
                            |> addCardToGuess (getCardId testPersonCard)
                            |> addCardToGuess (getCardId testWeaponCard)
                            |> addCardToGuess (getCardId testRoomCard)
                            |> finishGuess
                            |> extractGuess
                            |> addShowerToGuess showerId
                            |> addRevealedCardToGuess (getCardId testWeaponCard)

                    expectedShowerStatuses =
                        ( Just (MaybeHolding 1)
                        , Just Holding
                        , Just (MaybeHolding 1)
                        )

                    expectedOtherPlayerStatuses =
                        ( Just (MaybeHolding 0)
                        , Just NotHolding
                        , Just (MaybeHolding 0)
                        )

                    actualShowerStatuses =
                        ( getHoldingStatus testPlayers [ guess ] (getCardId testPersonCard) showerId
                        , getHoldingStatus testPlayers [ guess ] (getCardId testWeaponCard) showerId
                        , getHoldingStatus testPlayers [ guess ] (getCardId testRoomCard) showerId
                        )

                    actualOtherPlayerStatuses =
                        ( getHoldingStatus testPlayers [ guess ] (getCardId testPersonCard) otherPlayerId
                        , getHoldingStatus testPlayers [ guess ] (getCardId testWeaponCard) otherPlayerId
                        , getHoldingStatus testPlayers [ guess ] (getCardId testRoomCard) otherPlayerId
                        )
                in
                Expect.equal
                    ( expectedShowerStatuses, expectedOtherPlayerStatuses )
                    ( actualShowerStatuses, actualOtherPlayerStatuses )
        , test """
        If a player shows some card for a guess,
        and later is discovered to not be holding two of the cards from that guess,
        the player is definitely holding the third card from the guess.
        """ <|
            \_ ->
                let
                    playerId =
                        3

                    guessOne =
                        Clue.beginGuess 0
                            |> addCardToGuess "plum"
                            |> addCardToGuess "knife"
                            |> addCardToGuess "hall"
                            |> finishGuess
                            |> extractGuess
                            |> addShowerToGuess playerId

                    guessTwo =
                        Clue.beginGuess 1
                            |> addCardToGuess "plum"
                            |> addCardToGuess "knife"
                            |> addCardToGuess "conservatory"
                            |> finishGuess
                            |> extractGuess
                            |> addNoShowToGuess playerId

                    playerThreeHallStatus =
                        getHoldingStatus testPlayers [ guessOne, guessTwo ] "hall" playerId
                in
                Expect.equal (Just Holding) playerThreeHallStatus
        , test """
        When there are six players in the game,
        and a player is known to have three cards,
        they are not holding any other cards.
        """ <|
            \_ ->
                let
                    playerId =
                        3

                    guessOne =
                        Clue.beginGuess 0
                            |> addCardToGuess "plum"
                            |> addCardToGuess "knife"
                            |> addCardToGuess "hall"
                            |> finishGuess
                            |> extractGuess
                            |> addShowerToGuess playerId
                            |> addRevealedCardToGuess "plum"

                    guessTwo =
                        Clue.beginGuess 1
                            |> addCardToGuess "plum"
                            |> addCardToGuess "knife"
                            |> addCardToGuess "hall"
                            |> finishGuess
                            |> extractGuess
                            |> addShowerToGuess playerId
                            |> addRevealedCardToGuess "knife"

                    guessThree =
                        Clue.beginGuess 2
                            |> addCardToGuess "plum"
                            |> addCardToGuess "knife"
                            |> addCardToGuess "hall"
                            |> finishGuess
                            |> extractGuess
                            |> addShowerToGuess playerId
                            |> addRevealedCardToGuess "hall"

                    otherCardStatus =
                        getHoldingStatus testPlayers [ guessOne, guessTwo, guessThree ] "conservatory" playerId
                in
                Expect.equal (Just NotHolding) otherCardStatus
        , test """
        When there are three players in the game,
        and a player is known to have four cards,
        they are not holding any other cards.
        """ <|
            \_ ->
                let
                    threePlayers =
                        Player.noPlayers
                            |> Player.addNewPlayer "Zach"
                            |> Player.addNewPlayer "Allison"
                            |> Player.addNewPlayer "Bill"

                    playerId =
                        2

                    guessOne =
                        Clue.beginGuess 0
                            |> addCardToGuess "plum"
                            |> addCardToGuess "knife"
                            |> addCardToGuess "hall"
                            |> finishGuess
                            |> extractGuess
                            |> addShowerToGuess playerId
                            |> addRevealedCardToGuess "plum"

                    guessTwo =
                        Clue.beginGuess 1
                            |> addCardToGuess "green"
                            |> addCardToGuess "knife"
                            |> addCardToGuess "hall"
                            |> finishGuess
                            |> extractGuess
                            |> addShowerToGuess playerId
                            |> addRevealedCardToGuess "green"

                    guessThree =
                        Clue.beginGuess 0
                            |> addCardToGuess "mustard"
                            |> addCardToGuess "knife"
                            |> addCardToGuess "hall"
                            |> finishGuess
                            |> extractGuess
                            |> addShowerToGuess playerId
                            |> addRevealedCardToGuess "mustard"

                    guessFour =
                        Clue.beginGuess 1
                            |> addCardToGuess "white"
                            |> addCardToGuess "knife"
                            |> addCardToGuess "hall"
                            |> finishGuess
                            |> extractGuess
                            |> addShowerToGuess playerId
                            |> addRevealedCardToGuess "white"

                    otherCardStatus =
                        getHoldingStatus threePlayers [ guessOne, guessTwo, guessThree, guessFour ] "knife" playerId
                in
                Expect.equal (Just NotHolding) otherCardStatus
        ]
