module Main exposing (main)

import Browser exposing (element)
import Domain exposing (..)
import Facts exposing (Facts, HoldingStatus(..))
import GameBoard
import GameState exposing (GameState(..), SubjectOfInvestigation(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (takeWhile, takeWhileRight)
import Player exposing (Player, PlayerId, Players)



---- MODEL ----


type alias Model =
    { players : Players
    , gameState : GameState
    , facts : Facts
    , history : List CompleteGuess
    }


init : ( Model, Cmd Msg )
init =
    ( { players = Player.noPlayers
      , gameState = Setup ""
      , facts = Facts.initFacts
      , history = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = BuildPlayerName String
    | AddPlayer
    | StartGame
    | Investigate SubjectOfInvestigation
    | BeginGuess PlayerId
    | SetPersonGuess Person
    | SetWeaponGuess Weapon
    | SetRoomGuess Room
    | PlayerHasCard PlayerId Card
    | NoCardsToShow CompleteGuess PlayerId
    | ShowsSomeCard CompleteGuess PlayerId


buildPlayerName : Model -> String -> ( Model, Cmd Msg )
buildPlayerName model nameFragment =
    ( { model
        | gameState = Setup nameFragment
      }
    , Cmd.none
    )


addPlayer : Model -> ( Model, Cmd Msg )
addPlayer model =
    case model.gameState of
        Setup playerName ->
            ( { model
                | gameState = Setup ""
                , players = Player.addNewPlayer playerName model.players
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


startGame : Model -> ( Model, Cmd Msg )
startGame model =
    ( { model
        | gameState = Investigating People
        , facts = Facts.openingFacts allCards (Player.allIds model.players)
      }
    , Cmd.none
    )


beginGuess : Model -> PlayerId -> ( Model, Cmd Msg )
beginGuess model playerId =
    case model.gameState of
        Investigating p ->
            ( { model | gameState = Guessing playerId NothingIsSet }, Cmd.none )

        _ ->
            ( model, Cmd.none )


setPersonGuess : Model -> Person -> ( Model, Cmd Msg )
setPersonGuess model person =
    case model.gameState of
        Guessing guesser NothingIsSet ->
            ( { model | gameState = Guessing guesser (PersonIsSet person) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


setWeaponGuess : Model -> Weapon -> ( Model, Cmd Msg )
setWeaponGuess model weapon =
    case model.gameState of
        Guessing guesser (PersonIsSet person) ->
            ( { model | gameState = Guessing guesser (WeaponIsSet person weapon) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


buildCompleteGuess : PlayerId -> Person -> Weapon -> Room -> CompleteGuess
buildCompleteGuess guesser person weapon room =
    { guesser = guesser
    , person = person
    , weapon = weapon
    , room = room
    , noShows = []
    , shower = Nothing
    }


setRoomGuess : Model -> Room -> ( Model, Cmd Msg )
setRoomGuess model room =
    case model.gameState of
        Guessing guesser (WeaponIsSet person weapon) ->
            ( { model | gameState = Revealing (buildCompleteGuess guesser person weapon room) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


investigate : Model -> SubjectOfInvestigation -> ( Model, Cmd Msg )
investigate model subject =
    ( { model | gameState = Investigating subject }, Cmd.none )


playerHasCard : Model -> PlayerId -> Card -> ( Model, Cmd Msg )
playerHasCard model playerId card =
    let
        updatedFacts =
            model.facts
                |> Facts.setPlayerHasCard card playerId
                |> Facts.analyze (Player.allIds model.players) model.history

        nextGameState =
            Investigating (PlayerHand playerId)
    in
    ( { model | gameState = nextGameState, facts = updatedFacts }
    , Cmd.none
    )


allOtherPlayersHaveShownNoCards : CompleteGuess -> List PlayerId -> Bool
allOtherPlayersHaveShownNoCards guess players =
    List.length guess.noShows == List.length players - 1


noCardsToShow : Model -> CompleteGuess -> PlayerId -> ( Model, Cmd Msg )
noCardsToShow model guess playerId =
    let
        updatedGuess =
            { guess | noShows = playerId :: guess.noShows }

        goneAroundTheCircle =
            allOtherPlayersHaveShownNoCards updatedGuess (Player.allIds model.players)

        updatedHistory =
            if goneAroundTheCircle then
                guess :: model.history

            else
                model.history

        updatedFacts =
            model.facts
                |> Facts.setPlayerDoesNotHaveCard (PersonTag guess.person) playerId
                |> Facts.setPlayerDoesNotHaveCard (WeaponTag guess.weapon) playerId
                |> Facts.setPlayerDoesNotHaveCard (RoomTag guess.room) playerId
                |> Facts.analyze (Player.allIds model.players) updatedHistory

        updatedState =
            if goneAroundTheCircle then
                Investigating (PlayerHand updatedGuess.guesser)

            else
                Revealing updatedGuess
    in
    ( { model
        | gameState = updatedState
        , facts = updatedFacts
        , history = updatedHistory
      }
    , Cmd.none
    )


showsSomeCard : Model -> CompleteGuess -> PlayerId -> ( Model, Cmd Msg )
showsSomeCard model guess playerId =
    let
        updatedGuess =
            { guess | shower = Just playerId }

        updatedHistory =
            updatedGuess :: model.history

        updatedFacts =
            model.facts
                |> Facts.setPlayerMightHaveCard (PersonTag guess.person) playerId
                |> Facts.setPlayerMightHaveCard (WeaponTag guess.weapon) playerId
                |> Facts.setPlayerMightHaveCard (RoomTag guess.room) playerId
                |> Facts.analyze (Player.allIds model.players) updatedHistory

        updatedState =
            Investigating (PlayerHand playerId)
    in
    ( { model
        | gameState = updatedState
        , facts = updatedFacts
        , history = updatedHistory
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BuildPlayerName nameFragment ->
            buildPlayerName model nameFragment

        AddPlayer ->
            addPlayer model

        StartGame ->
            startGame model

        Investigate subject ->
            investigate model subject

        BeginGuess playerId ->
            beginGuess model playerId

        SetPersonGuess person ->
            setPersonGuess model person

        SetWeaponGuess weapon ->
            setWeaponGuess model weapon

        SetRoomGuess room ->
            setRoomGuess model room

        PlayerHasCard playerId card ->
            playerHasCard model playerId card

        NoCardsToShow guess playerId ->
            noCardsToShow model guess playerId

        ShowsSomeCard guess playerId ->
            showsSomeCard model guess playerId



---- VIEW ----


title : Html msg
title =
    h1 [] [ text "Clue!" ]


addPlayerToGame : String -> List (Html Msg)
addPlayerToGame nameFragment =
    [ input [ type_ "text", placeholder "Name", value nameFragment, onInput BuildPlayerName ] []
    , button [ onClick AddPlayer ] [ text "Add player" ]
    , button [ onClick StartGame ] [ text "Start" ]
    ]


listAddedPlayerNames : List Player -> List (Html msg)
listAddedPlayerNames players =
    List.map (\player -> p [] [ text (Player.getName player) ]) players


setupNewGame : Model -> Html Msg
setupNewGame model =
    case model.gameState of
        Setup nameFragment ->
            div []
                ([]
                    ++ addPlayerToGame nameFragment
                    ++ listAddedPlayerNames (Player.allPlayers model.players)
                )

        _ ->
            div [] []


cardOption : Card -> Html Msg
cardOption card =
    let
        clickHandler =
            case card of
                PersonTag person ->
                    SetPersonGuess person

                WeaponTag weapon ->
                    SetWeaponGuess weapon

                RoomTag room ->
                    SetRoomGuess room
    in
    a [ class "button", onClick clickHandler ] [ text (Domain.displayCard card) ]


selectCard : List Card -> Html Msg
selectCard cards =
    div [] (List.map cardOption cards)


showerOptionButtons : CompleteGuess -> PlayerId -> List (Html Msg)
showerOptionButtons guess playerId =
    if List.member playerId guess.noShows then
        []

    else
        [ button [ class "button", onClick (ShowsSomeCard guess playerId) ] [ text "yes" ]
        , button [ class "button", onClick (NoCardsToShow guess playerId) ] [ text "no" ]
        ]


showerOption : CompleteGuess -> Player -> List (Html Msg)
showerOption guess player =
    [ dt [] [ text (Player.getName player) ]
    , dd [] (showerOptionButtons guess (Player.getId player))
    ]


displayGuess : CompleteGuess -> String
displayGuess guess =
    Domain.displayCard (PersonTag guess.person)
        ++ " :: "
        ++ Domain.displayCard (WeaponTag guess.weapon)
        ++ " :: "
        ++ Domain.displayCard (RoomTag guess.room)


otherPlayers : PlayerId -> List Player -> List Player
otherPlayers guesserId allPlayers =
    let
        isNotGuesser =
            \player -> guesserId /= Player.getId player
    in
    takeWhileRight isNotGuesser allPlayers ++ takeWhile isNotGuesser allPlayers


renderShowerOptions : CompleteGuess -> List Player -> Html Msg
renderShowerOptions guess players =
    div []
        [ h3 [] [ text (displayGuess guess) ]
        , dl [] (List.concatMap (showerOption guess) (otherPlayers guess.guesser players))
        ]


selectCards : InProgressGuess -> Html Msg
selectCards guess =
    case guess of
        NothingIsSet ->
            selectCard personCards

        PersonIsSet _ ->
            selectCard weaponCards

        WeaponIsSet _ _ ->
            selectCard roomCards


guessingForm : Model -> Html Msg
guessingForm model =
    case model.gameState of
        Guessing _ inProgressGuess ->
            selectCards inProgressGuess

        _ ->
            div [] []


revealingForm : Model -> Html Msg
revealingForm model =
    case model.gameState of
        Revealing guess ->
            renderShowerOptions guess (Player.allPlayers model.players)

        _ ->
            div [] []


playerCardStatusDescListValue : Facts -> PlayerId -> Card -> List (Html Msg)
playerCardStatusDescListValue facts playerId card =
    let
        holdingStatus =
            Facts.getHoldingStatus facts card playerId

        statusText =
            text (Facts.displayHoldingStatus holdingStatus)
    in
    case holdingStatus of
        Just (MaybeHolding count) ->
            [ statusText
            , button [ onClick (PlayerHasCard playerId card) ] [ text "reveal" ]
            ]

        _ ->
            [ statusText ]


playerCardStatusAsDescriptionListEntry : Facts -> PlayerId -> Card -> List (Html Msg)
playerCardStatusAsDescriptionListEntry facts playerId card =
    [ dt [] [ text (Domain.displayCard card) ]
    , dd [] (playerCardStatusDescListValue facts playerId card)
    ]


playerView : PlayerId -> Model -> Html Msg
playerView playerId model =
    let
        playerCardStatusAsDL =
            playerCardStatusAsDescriptionListEntry model.facts playerId
    in
    div []
        [ h1 [] [ text (Player.lookupName playerId model.players) ]
        , p [] [ a [ class "button", onClick (BeginGuess playerId) ] [ text "Begin guess" ] ]
        , dl [] (List.concatMap playerCardStatusAsDL personCards)
        , dl [] (List.concatMap playerCardStatusAsDL weaponCards)
        , dl [] (List.concatMap playerCardStatusAsDL roomCards)
        ]


investigatingView : SubjectOfInvestigation -> Model -> Html Msg
investigatingView subject model =
    case subject of
        PlayerHand playerId ->
            playerView playerId model

        People ->
            GameBoard.render personCards model.facts model.players

        Weapons ->
            GameBoard.render weaponCards model.facts model.players

        Rooms ->
            GameBoard.render roomCards model.facts model.players


renderMainDisplay : Model -> Html Msg
renderMainDisplay model =
    case model.gameState of
        Investigating subject ->
            investigatingView subject model

        Guessing _ _ ->
            guessingForm model

        Revealing _ ->
            revealingForm model

        Setup _ ->
            div [] []


mainDisplay : Model -> Html Msg
mainDisplay model =
    renderMainDisplay model


investigatePlayerButton : Player -> Html Msg
investigatePlayerButton player =
    a [ class "button", onClick (Investigate (PlayerHand (Player.getId player))) ] [ text (Player.getName player) ]


playerSelect : Model -> Html Msg
playerSelect model =
    div [] (List.map investigatePlayerButton (Player.allPlayers model.players))


investigateCardTypeButton : SubjectOfInvestigation -> Html Msg
investigateCardTypeButton cardType =
    a [ class "button", onClick (Investigate cardType) ] [ text (Debug.toString cardType) ]


cardTypeSelect : Model -> Html Msg
cardTypeSelect model =
    div [] (List.map investigateCardTypeButton [ People, Weapons, Rooms ])


activeGame : Model -> Html Msg
activeGame model =
    case model.gameState of
        Setup _ ->
            div [] []

        _ ->
            div []
                [ playerSelect model
                , cardTypeSelect model
                , mainDisplay model
                ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ title
        , setupNewGame model
        , activeGame model
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
