module GameState exposing (GameState(..), SubjectOfInvestigation(..))

import Domain exposing (CompleteGuess, InProgressGuess)
import Player exposing (PlayerId)


type SubjectOfInvestigation
    = PlayerHand PlayerId
    | People
    | Weapons
    | Rooms


type GameState
    = Setup String
    | Guessing PlayerId InProgressGuess
    | Revealing CompleteGuess
    | Investigating SubjectOfInvestigation
