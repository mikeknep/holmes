module GameState exposing (GameState(..), SubjectOfInvestigation(..))

import Domain exposing (CompleteGuess, InProgressGuess, Player)


type SubjectOfInvestigation
    = PlayerHand Player
    | People
    | Weapons
    | Rooms


type GameState
    = Guessing Player InProgressGuess
    | Revealing CompleteGuess
    | Investigating SubjectOfInvestigation