module GameBoard exposing (render)

import Clue exposing (Card)
import Conclusions exposing (Conclusions, HoldingStatus(..))
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class)
import Player exposing (Player, PlayerId, Players)


render : List Card -> Conclusions -> Players -> Html msg
render cards conclusions players =
    div []
        [ table [ class "table" ]
            ([ headerRow (Player.allPlayers players) ]
                ++ List.map (cardRow conclusions (Dict.keys players)) cards
            )
        ]


blankCell : Html msg
blankCell =
    td [] []


blankRow : List Player -> Html msg
blankRow players =
    tr [] (blankCell :: List.map (\_ -> blankCell) players)


headerRow : List Player -> Html msg
headerRow players =
    tr [] (blankCell :: List.map playerColumnHeader players)


playerColumnHeader : Player -> Html msg
playerColumnHeader player =
    th [] [ text (Player.getName player) ]


displayHoldingStatus : HoldingStatus -> String
displayHoldingStatus status =
    case status of
        NotHolding ->
            "No"

        MaybeHolding count ->
            "Maybe (" ++ String.fromInt count ++ ")"

        Holding ->
            "Yes"


cardPlayerCell : Conclusions -> Card -> PlayerId -> Html msg
cardPlayerCell conclusions card playerId =
    let
        holdingStatus =
            Conclusions.getHoldingStatus conclusions (Clue.getCardId card) playerId
    in
    td [] [ text (displayHoldingStatus holdingStatus) ]


cardCell : Card -> Html msg
cardCell card =
    td [] [ text (Clue.displayCard card) ]


cardRow : Conclusions -> List PlayerId -> Card -> Html msg
cardRow conclusions playerIds card =
    tr [] (cardCell card :: List.map (cardPlayerCell conclusions card) playerIds)
