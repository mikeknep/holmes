module Main exposing (CardholdingStatus, HoldingStatus(..), ParticipatingPlayer)


type HoldingStatus
    = Unknown
    | NotHeld
    | PossiblyHeld
    | Held


type alias CardholdingStatus =
    { card : Card
    , holdingStatus : HoldingStatus
    }


type alias ParticipatingPlayer =
    { player : Player
    , cardholdingStatuses : List CardholdingStatus
    }
