module Setup exposing
    ( Setup
    , addPlayer
    , beginSetup
    , buildPlayerName
    , completeSetup
    , getDisadvantagedPlayers
    , getNameFragment
    , getPlayerNames
    , setDisadvantagedPlayer
    )

import Player exposing (Players)


type alias SetupDetails =
    { playerNames : List String
    , nameFragment : String
    , disadvantagedPlayers : Maybe ( String, Maybe String )
    }


type Setup
    = Setup SetupDetails


beginSetup : Setup
beginSetup =
    Setup
        { playerNames = []
        , nameFragment = ""
        , disadvantagedPlayers = Nothing
        }


addPlayer : Setup -> Setup
addPlayer (Setup details) =
    Setup
        { details
            | playerNames = details.nameFragment :: details.playerNames
            , nameFragment = ""
        }


buildPlayerName : String -> Setup -> Setup
buildPlayerName nameFragment (Setup details) =
    Setup { details | nameFragment = nameFragment }


getPlayerNames : Setup -> List String
getPlayerNames (Setup { playerNames }) =
    playerNames


getNameFragment : Setup -> String
getNameFragment (Setup { nameFragment }) =
    nameFragment


getDisadvantagedPlayers : Setup -> List String
getDisadvantagedPlayers (Setup { disadvantagedPlayers }) =
    case disadvantagedPlayers of
        Nothing ->
            []

        Just ( player, Nothing ) ->
            [ player ]

        Just ( player1, Just player2 ) ->
            [ player1, player2 ]


setDisadvantagedPlayer : String -> Setup -> Setup
setDisadvantagedPlayer name (Setup details) =
    case details.disadvantagedPlayers of
        Nothing ->
            Setup { details | disadvantagedPlayers = Just ( name, Nothing ) }

        Just ( player, Nothing ) ->
            Setup { details | disadvantagedPlayers = Just ( player, Just name ) }

        Just ( _, Just _ ) ->
            Setup details


completeSetup : Setup -> Result Setup Players
completeSetup (Setup setupDetails) =
    case List.length setupDetails.playerNames of
        3 ->
            Ok (buildEvenGamePlayers setupDetails.playerNames 6)

        6 ->
            Ok (buildEvenGamePlayers setupDetails.playerNames 3)

        _ ->
            completeUnevenGameSetup setupDetails


completeUnevenGameSetup : SetupDetails -> Result Setup Players
completeUnevenGameSetup setupDetails =
    case setupDetails.disadvantagedPlayers of
        Just ( name1, Just name2 ) ->
            Ok (buildUnevenPlayers setupDetails.playerNames [ name1, name2 ])

        _ ->
            Err (Setup setupDetails)


buildEvenGamePlayers : List String -> Int -> Players
buildEvenGamePlayers names numberOfCards =
    List.foldr (Player.addNewPlayer numberOfCards) Player.noPlayers names


buildUnevenPlayers : List String -> List String -> Players
buildUnevenPlayers allPlayerNames disadvantagedPlayerNames =
    List.foldr (addPlayerInUnevenGame (List.length allPlayerNames) disadvantagedPlayerNames) Player.noPlayers allPlayerNames


addPlayerInUnevenGame : Int -> List String -> String -> Players -> Players
addPlayerInUnevenGame totalPlayerCount disadvantagedPlayers playerName players =
    let
        ( advantageCardCount, disadvantageCardCount ) =
            getCardCountOptionsForNPlayerGame totalPlayerCount

        numberOfCards =
            if List.member playerName disadvantagedPlayers then
                disadvantageCardCount

            else
                advantageCardCount
    in
    Player.addNewPlayer numberOfCards playerName players


getCardCountOptionsForNPlayerGame : Int -> ( Int, Int )
getCardCountOptionsForNPlayerGame n =
    case n of
        4 ->
            ( 5, 4 )

        5 ->
            ( 4, 3 )

        _ ->
            ( 0, 0 )
