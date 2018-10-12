module FactsPresenter exposing (displayHoldingStatus)

import Facts exposing (HoldingStatus(..))


displayHoldingStatus : Maybe HoldingStatus -> String
displayHoldingStatus holdingStatus =
    case holdingStatus of
        Just NotHolding ->
            "No"

        Just (MaybeHolding count) ->
            "Maybe: " ++ String.fromInt count

        Just Holding ->
            "Yes!"

        _ ->
            "This should be impossible"
