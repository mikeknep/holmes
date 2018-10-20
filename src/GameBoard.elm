module GameBoard exposing (render)

import CardPresenter
import Dict exposing (..)
import Domain exposing (..)
import Facts exposing (Facts, HoldingStatus(..))
import FactsPresenter
import Html exposing (..)
import Html.Attributes exposing (class)


render : List Card -> Facts -> Dict PlayerId Player -> Html msg
render cards facts players =
    div []
        [ table [ class "table" ]
            ([ headerRow (Dict.values players) ]
                ++ List.map (cardRow facts (Dict.keys players)) cards
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
    th [] [ text player.name ]


cardPlayerCell : Facts -> Card -> PlayerId -> Html msg
cardPlayerCell facts card playerId =
    let
        holdingStatus =
            Facts.getHoldingStatus facts card playerId
    in
    td [] [ text (FactsPresenter.displayHoldingStatus holdingStatus) ]


cardCell : Card -> Html msg
cardCell card =
    td [] [ text (CardPresenter.displayCard card) ]


cardRow : Facts -> List PlayerId -> Card -> Html msg
cardRow facts playerIds card =
    tr [] (cardCell card :: List.map (cardPlayerCell facts card) playerIds)
