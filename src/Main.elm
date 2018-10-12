module Main exposing (main)

import Browser exposing (element)
import CardPresenter
import Domain exposing (..)
import Facts exposing (Facts, HoldingStatus(..))
import FactsPresenter
import GameBoard
import GameState exposing (GameState(..), SubjectOfInvestigation(..))
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra exposing (takeWhile, takeWhileRight)



---- MODEL ----


type alias Model =
    { players : List Player
    , gameState : GameState
    , facts : Facts
    , history : List CompleteGuess
    }


init : ( Model, Cmd Msg )
init =
    ( { players = []
      , gameState = Investigating People
      , facts = Facts.initFacts
      , history = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SetPlayers Int
    | ResetGame
    | Investigate SubjectOfInvestigation
    | BeginGuess Player
    | SetPersonGuess Person
    | SetWeaponGuess Weapon
    | SetRoomGuess Room
    | PlayerHasCard Player Card
    | NoCardsToShow CompleteGuess Player
    | ShowsSomeCard CompleteGuess Player


setPlayers : Model -> Int -> ( Model, Cmd Msg )
setPlayers model playerCount =
    let
        gamePlayers =
            List.take playerCount possiblePlayers

        allCards =
            personCards ++ weaponCards ++ roomCards
    in
    ( { model
        | players = gamePlayers
        , facts = Facts.openingFacts allCards gamePlayers
      }
    , Cmd.none
    )


beginGuess : Model -> Player -> ( Model, Cmd Msg )
beginGuess model player =
    case model.gameState of
        Investigating p ->
            ( { model | gameState = Guessing player NothingIsSet }, Cmd.none )

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


buildCompleteGuess : Player -> Person -> Weapon -> Room -> CompleteGuess
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


playerHasCard : Model -> Player -> Card -> ( Model, Cmd Msg )
playerHasCard model player card =
    let
        updatedFacts =
            Facts.setPlayerHasCard card player model.facts

        nextGameState =
            Investigating (PlayerHand player)
    in
    ( { model | gameState = nextGameState, facts = updatedFacts }
    , Cmd.none
    )


allOtherPlayersHaveShownNoCards : CompleteGuess -> List Player -> Bool
allOtherPlayersHaveShownNoCards guess players =
    List.length guess.noShows == List.length players - 1


noCardsToShow : Model -> CompleteGuess -> Player -> ( Model, Cmd Msg )
noCardsToShow model guess player =
    let
        updatedGuess =
            { guess | noShows = player :: guess.noShows }

        goneAroundTheCircle =
            allOtherPlayersHaveShownNoCards updatedGuess model.players

        updatedHistory =
            if goneAroundTheCircle then
                guess :: model.history

            else
                model.history

        updatedFacts =
            model.facts
                |> Facts.setPlayerDoesNotHaveCard (PersonTag guess.person) player
                |> Facts.setPlayerDoesNotHaveCard (WeaponTag guess.weapon) player
                |> Facts.setPlayerDoesNotHaveCard (RoomTag guess.room) player

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


showsSomeCard : Model -> CompleteGuess -> Player -> ( Model, Cmd Msg )
showsSomeCard model guess player =
    let
        updatedGuess =
            { guess | shower = Just player }

        updatedHistory =
            updatedGuess :: model.history

        updatedFacts =
            model.facts
                |> Facts.setPlayerMightHaveCard (PersonTag guess.person) player
                |> Facts.setPlayerMightHaveCard (WeaponTag guess.weapon) player
                |> Facts.setPlayerMightHaveCard (RoomTag guess.room) player

        updatedState =
            Investigating (PlayerHand player)
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
        SetPlayers playerCount ->
            setPlayers model playerCount

        ResetGame ->
            init

        Investigate subject ->
            investigate model subject

        BeginGuess player ->
            beginGuess model player

        SetPersonGuess person ->
            setPersonGuess model person

        SetWeaponGuess weapon ->
            setWeaponGuess model weapon

        SetRoomGuess room ->
            setRoomGuess model room

        PlayerHasCard player card ->
            playerHasCard model player card

        NoCardsToShow guess player ->
            noCardsToShow model guess player

        ShowsSomeCard guess player ->
            showsSomeCard model guess player



---- VIEW ----


title : Html msg
title =
    h1 [] [ text "Clue!" ]


playerCountButton : Int -> Html Msg
playerCountButton numberOfPlayers =
    button [ onClick (SetPlayers numberOfPlayers) ] [ text (String.fromInt numberOfPlayers) ]


selectNumberOfPlayers : Html Msg
selectNumberOfPlayers =
    div []
        (h3 [] [ text "Select number of players" ]
            :: List.map playerCountButton possibleNumbersOfPlayers
        )


resetGame : Html Msg
resetGame =
    button [ onClick ResetGame ] [ text "Reset" ]


setupNewGame : Model -> Html Msg
setupNewGame model =
    case model.players of
        [] ->
            selectNumberOfPlayers

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
    a [ class "button", onClick clickHandler ] [ text (CardPresenter.displayCard card) ]


selectCard : List Card -> Html Msg
selectCard cards =
    div [] (List.map cardOption cards)


showerOption : CompleteGuess -> Player -> List (Html Msg)
showerOption guess player =
    [ dt [] [ text player.name ]
    , dd []
        [ button [ onClick (ShowsSomeCard guess player) ] [ text "yes" ]
        , button [ onClick (NoCardsToShow guess player) ] [ text "no" ]
        ]
    ]


displayGuess : CompleteGuess -> String
displayGuess guess =
    guess.guesser.name
        ++ " :: "
        ++ CardPresenter.displayCard (PersonTag guess.person)
        ++ " :: "
        ++ CardPresenter.displayCard (WeaponTag guess.weapon)
        ++ " :: "
        ++ CardPresenter.displayCard (RoomTag guess.room)


otherPlayers : Player -> List Player -> List Player
otherPlayers guesser allPlayers =
    let
        isNotGuesser =
            \player -> player /= guesser
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
            renderShowerOptions guess model.players

        _ ->
            div [] []


playerCardStatusDescListValue : Facts -> Player -> Card -> List (Html Msg)
playerCardStatusDescListValue facts player card =
    let
        holdingStatus =
            Facts.getHoldingStatus facts card player

        statusText =
            text (FactsPresenter.displayHoldingStatus holdingStatus)
    in
    case holdingStatus of
        Just (MaybeHolding count) ->
            [ statusText
            , button [ onClick (PlayerHasCard player card) ] [ text "reveal" ]
            ]

        _ ->
            [ statusText ]


playerCardStatusAsDescriptionListEntry : Facts -> Player -> Card -> List (Html Msg)
playerCardStatusAsDescriptionListEntry facts player card =
    [ dt [] [ text (CardPresenter.displayCard card) ]
    , dd [] (playerCardStatusDescListValue facts player card)
    ]


playerView : Player -> Model -> Html Msg
playerView player model =
    let
        playerCardStatusAsDL =
            playerCardStatusAsDescriptionListEntry model.facts player
    in
    div []
        [ h1 [] [ text player.name ]
        , p [] [ a [ class "button", onClick (BeginGuess player) ] [ text "Begin guess" ] ]
        , dl [] (List.concatMap playerCardStatusAsDL personCards)
        , dl [] (List.concatMap playerCardStatusAsDL weaponCards)
        , dl [] (List.concatMap playerCardStatusAsDL roomCards)
        ]


investigatingView : SubjectOfInvestigation -> Model -> Html Msg
investigatingView subject model =
    case subject of
        PlayerHand player ->
            playerView player model

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


mainDisplay : Model -> Html Msg
mainDisplay model =
    case model.players of
        [] ->
            div [] []

        _ ->
            renderMainDisplay model


investigatePlayerButton : Player -> Html Msg
investigatePlayerButton player =
    a [ class "button", onClick (Investigate (PlayerHand player)) ] [ text player.name ]


playerSelect : Model -> Html Msg
playerSelect model =
    div [] (List.map investigatePlayerButton model.players)


investigateCardTypeButton : SubjectOfInvestigation -> Html Msg
investigateCardTypeButton cardType =
    a [ class "button", onClick (Investigate cardType) ] [ text (Debug.toString cardType) ]


cardTypeSelect : Model -> Html Msg
cardTypeSelect model =
    div [] (List.map investigateCardTypeButton [ People, Weapons, Rooms ])


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ title
        , setupNewGame model
        , playerSelect model
        , cardTypeSelect model
        , mainDisplay model
        , resetGame
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
