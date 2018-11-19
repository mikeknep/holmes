module Player exposing
    ( Player
    , PlayerId
    , Players
    , addNewPlayer
    , allIds
    , allPlayers
    , getId
    , getName
    , lookupName
    , noPlayers
    )

import Dict exposing (..)


type Players
    = Players PlayersDict


type alias PlayersDict =
    Dict PlayerId Player


noPlayers : Players
noPlayers =
    Players Dict.empty


addNewPlayer : String -> Int -> Players -> Players
addNewPlayer name cardCount (Players players) =
    if String.isEmpty name then
        Players players

    else
        let
            id =
                Dict.size players

            newPlayer =
                Player
                    { id = id
                    , name = name
                    , numberOfCardsInHand = cardCount
                    }
        in
        players
            |> Dict.insert id newPlayer
            |> Players


allIds : Players -> List PlayerId
allIds (Players players) =
    Dict.keys players


allPlayers : Players -> List Player
allPlayers (Players players) =
    Dict.values players


type alias PlayerId =
    Int


type Player
    = Player PlayerDetails


type alias PlayerDetails =
    { id : PlayerId
    , name : String
    , numberOfCardsInHand : Int
    }


getId : Player -> PlayerId
getId (Player { id }) =
    id


getName : Player -> String
getName (Player { name }) =
    name


lookupName : PlayerId -> Players -> String
lookupName id (Players players) =
    Dict.get id players
        |> Maybe.map getName
        |> Maybe.withDefault "???"
