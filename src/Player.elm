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


addNewPlayer : String -> Players -> Players
addNewPlayer name (Players players) =
    if String.length name == 0 then
        Players players

    else
        let
            id =
                Dict.size players
        in
        Players (Dict.insert id (createPlayer id name) players)


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


createPlayer : PlayerId -> String -> Player
createPlayer playerId name =
    Player
        { id = playerId
        , name = name
        , numberOfCardsInHand = 4
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
