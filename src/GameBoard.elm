module GameBoard exposing (render)

import CardPresenter
import Domain exposing (..)
import Facts exposing (Facts, HoldingStatus(..))
import FactsPresenter
import Html exposing (..)
import Html.Attributes exposing (class)


render : List Card -> Facts -> List Player -> Html msg
render cards facts players =
    div []
        [ table [ class "table" ]
            ([ headerRow players ]
                ++ List.map (cardRow facts players) cards
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


cardPlayerCell : Facts -> Card -> Player -> Html msg
cardPlayerCell facts card player =
    let
        holdingStatus =
            Facts.getHoldingStatus facts card player
    in
    td [] [ text (FactsPresenter.displayHoldingStatus holdingStatus) ]


cardCell : Card -> Html msg
cardCell card =
    td [] [ text (CardPresenter.displayCard card) ]


cardRow : Facts -> List Player -> Card -> Html msg
cardRow facts players card =
    tr [] (cardCell card :: List.map (cardPlayerCell facts card) players)
