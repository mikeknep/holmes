module Main exposing (main)

import Browser exposing (element)
import Clue exposing (Card, CardId, CompleteGuess, GuessHistory, IncompleteGuess, RevealHistory)
import Conclusions exposing (Conclusions, HoldingStatus(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (takeWhile, takeWhileRight)
import Player exposing (Player, PlayerId, Players)


type SubjectOfInvestigation
    = PlayerHand PlayerId
    | People
    | Weapons
    | Rooms


type GameState
    = Setup String
    | Guessing IncompleteGuess
    | Revealing CompleteGuess
    | Investigating SubjectOfInvestigation



---- MODEL ----


type alias Model =
    { players : Players
    , gameState : GameState
    , guessHistory : GuessHistory
    , revealHistory : RevealHistory
    }


init : ( Model, Cmd Msg )
init =
    ( { players = Player.noPlayers
      , gameState = Setup ""
      , guessHistory = Clue.noGuesses
      , revealHistory = Clue.noReveals
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = BuildPlayerName String
    | AddPlayer Int
    | StartGame
    | Investigate SubjectOfInvestigation
    | BeginGuess PlayerId
    | AddCardToGuess CardId
    | NoCardsToShow PlayerId
    | ShowsSomeCard PlayerId
    | SetRevealedCard CardId PlayerId
    | EndGuess


buildPlayerName : Model -> String -> ( Model, Cmd Msg )
buildPlayerName model nameFragment =
    ( { model
        | gameState = Setup nameFragment
      }
    , Cmd.none
    )


addPlayer : Model -> Int -> ( Model, Cmd Msg )
addPlayer model cardCount =
    case model.gameState of
        Setup playerName ->
            ( { model
                | gameState = Setup ""
                , players = Player.addNewPlayer playerName cardCount model.players
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


startGame : Model -> ( Model, Cmd Msg )
startGame model =
    ( { model | gameState = Investigating People }
    , Cmd.none
    )


beginGuess : Model -> PlayerId -> ( Model, Cmd Msg )
beginGuess model playerId =
    ( { model | gameState = Guessing (Clue.beginGuess playerId) }
    , Cmd.none
    )


addCardToGuess : Model -> CardId -> ( Model, Cmd Msg )
addCardToGuess model cardId =
    case model.gameState of
        Guessing guess ->
            let
                maybeFinishedGuess =
                    guess
                        |> Clue.addCardToGuess cardId
                        |> Clue.finishGuess

                updatedGameState =
                    case maybeFinishedGuess of
                        Ok completeGuess ->
                            Revealing completeGuess

                        Err incompleteGuess ->
                            Guessing incompleteGuess
            in
            ( { model | gameState = updatedGameState }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


investigate : Model -> SubjectOfInvestigation -> ( Model, Cmd Msg )
investigate model subject =
    ( { model | gameState = Investigating subject }, Cmd.none )


allOtherPlayersHaveShownNoCards : CompleteGuess -> Int -> Bool
allOtherPlayersHaveShownNoCards guess numberOfPlayers =
    List.length (Clue.getNoShows guess) == numberOfPlayers - 1


noCardsToShow : Model -> PlayerId -> ( Model, Cmd Msg )
noCardsToShow model playerId =
    case model.gameState of
        Revealing guess ->
            let
                updatedGuess =
                    Clue.addNoShowToGuess playerId guess

                numberOfPlayers =
                    List.length (Player.allIds model.players)

                goneAroundTheCircle =
                    allOtherPlayersHaveShownNoCards updatedGuess numberOfPlayers

                updatedHistory =
                    if goneAroundTheCircle then
                        Clue.addGuessToHistory updatedGuess model.guessHistory

                    else
                        model.guessHistory

                updatedState =
                    if goneAroundTheCircle then
                        Investigating People

                    else
                        Revealing updatedGuess
            in
            ( { model
                | gameState = updatedState
                , guessHistory = updatedHistory
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


showsSomeCard : Model -> PlayerId -> ( Model, Cmd Msg )
showsSomeCard model playerId =
    case model.gameState of
        Revealing guess ->
            let
                updatedGuess =
                    Clue.addShowerToGuess playerId guess

                updatedGuessHistory =
                    Clue.addGuessToHistory updatedGuess model.guessHistory

                updatedState =
                    Revealing updatedGuess
            in
            ( { model
                | gameState = updatedState
                , guessHistory = updatedGuessHistory
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


setRevealedCard : Model -> CardId -> PlayerId -> ( Model, Cmd Msg )
setRevealedCard model cardId playerId =
    case model.gameState of
        Revealing guess ->
            ( { model
                | gameState = Investigating People
                , revealHistory = Clue.addRevealToHistory (Clue.createReveal cardId playerId) model.revealHistory
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


endGuess : Model -> ( Model, Cmd Msg )
endGuess model =
    ( { model | gameState = Investigating People }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BuildPlayerName nameFragment ->
            buildPlayerName model nameFragment

        AddPlayer cardCount ->
            addPlayer model cardCount

        StartGame ->
            startGame model

        Investigate subject ->
            investigate model subject

        BeginGuess playerId ->
            beginGuess model playerId

        AddCardToGuess cardId ->
            addCardToGuess model cardId

        NoCardsToShow playerId ->
            noCardsToShow model playerId

        ShowsSomeCard playerId ->
            showsSomeCard model playerId

        SetRevealedCard cardId playerId ->
            setRevealedCard model cardId playerId

        EndGuess ->
            endGuess model



---- VIEW ----


title : Html msg
title =
    h1 [] [ text "Clue!" ]


addPlayerToGame : String -> List (Html Msg)
addPlayerToGame nameFragment =
    [ input [ type_ "text", placeholder "Name", value nameFragment, onInput BuildPlayerName ] []
    , button [ onClick (AddPlayer 3) ] [ text "3" ]
    , button [ onClick (AddPlayer 4) ] [ text "4" ]
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
    a [ class "button", onClick (AddCardToGuess (Clue.getCardId card)) ] [ text (Clue.displayCard card) ]


selectCard : List Card -> Html Msg
selectCard cards =
    div [] (List.map cardOption cards)


showerOptionButtons : CompleteGuess -> PlayerId -> List (Html Msg)
showerOptionButtons guess playerId =
    if List.member playerId (Clue.getNoShows guess) then
        []

    else
        [ button [ class "button", onClick (ShowsSomeCard playerId) ] [ text "yes" ]
        , button [ class "button", onClick (NoCardsToShow playerId) ] [ text "no" ]
        ]


showerOption : CompleteGuess -> Player -> List (Html Msg)
showerOption guess player =
    [ dt [] [ text (Player.getName player) ]
    , dd [] (showerOptionButtons guess (Player.getId player))
    ]


displayGuess : CompleteGuess -> String
displayGuess guess =
    Clue.getCardIdsFromGuess guess
        |> List.map Clue.displayCardWithId
        |> List.intersperse " :: "
        |> List.foldr (++) ""


otherPlayers : PlayerId -> List Player -> List Player
otherPlayers guesserId allPlayers =
    let
        isNotGuesser =
            \player -> guesserId /= Player.getId player
    in
    takeWhileRight isNotGuesser allPlayers ++ takeWhile isNotGuesser allPlayers


renderShowerOptions : CompleteGuess -> List Player -> List (Html Msg)
renderShowerOptions guess players =
    [ h3 [] [ text (displayGuess guess) ]
    , dl [] (List.concatMap (showerOption guess) (otherPlayers (Clue.getGuesser guess) players))
    ]


revealedCardIsUnknown : Html Msg
revealedCardIsUnknown =
    a [ class "button", onClick EndGuess ] [ text "Unknown" ]


revealedCardOption : PlayerId -> CardId -> Html Msg
revealedCardOption playerId cardId =
    a [ class "button", onClick (SetRevealedCard cardId playerId) ] [ text (Clue.displayCardWithId cardId) ]


renderRevealedCardOptions : CompleteGuess -> List (Html Msg)
renderRevealedCardOptions guess =
    case Clue.getShower guess of
        Just showerId ->
            revealedCardIsUnknown
                :: List.map (revealedCardOption showerId) (Clue.getCardIdsFromGuess guess)

        Nothing ->
            []


guessingForm : Conclusions -> Model -> Html Msg
guessingForm conclusions model =
    case model.gameState of
        Guessing guess ->
            let
                cards =
                    Clue.getCardOptionsForGuess guess
            in
            div []
                [ selectCard cards
                , renderGameBoard cards conclusions model.players
                ]

        _ ->
            div [] []


revealingForm : Model -> Html Msg
revealingForm model =
    case model.gameState of
        Revealing guess ->
            div []
                (renderShowerOptions guess (Player.allPlayers model.players)
                    ++ renderRevealedCardOptions guess
                )

        _ ->
            div [] []


displayHoldingStatus : HoldingStatus -> String
displayHoldingStatus status =
    case status of
        NotHolding ->
            "No"

        MaybeHolding count ->
            "Maybe (" ++ String.fromInt count ++ ")"

        Holding ->
            "Yes"


playerCardStatusAsDescriptionListEntry : Conclusions -> PlayerId -> Card -> List (Html Msg)
playerCardStatusAsDescriptionListEntry conclusions playerId card =
    let
        holdingStatus =
            Conclusions.getHoldingStatus conclusions (Clue.getCardId card) playerId
                |> displayHoldingStatus
    in
    [ dt [] [ text (Clue.displayCard card) ]
    , dd [] [ text holdingStatus ]
    ]


playerView : Conclusions -> PlayerId -> Players -> Html Msg
playerView conclusions playerId players =
    let
        playerCardStatusAsDL =
            playerCardStatusAsDescriptionListEntry conclusions playerId
    in
    div []
        [ h1 [] [ text (Player.lookupName playerId players) ]
        , p [] [ a [ class "button", onClick (BeginGuess playerId) ] [ text "Begin guess" ] ]
        , dl [] (List.concatMap playerCardStatusAsDL Clue.personCards)
        , dl [] (List.concatMap playerCardStatusAsDL Clue.weaponCards)
        , dl [] (List.concatMap playerCardStatusAsDL Clue.roomCards)
        ]


renderGameBoard : List Card -> Conclusions -> Players -> Html msg
renderGameBoard cards conclusions players =
    div []
        [ table [ class "table" ]
            ([ headerRow (Player.allPlayers players) ]
                ++ List.map (cardRow conclusions (Player.allIds players)) cards
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


investigatingView : Conclusions -> SubjectOfInvestigation -> Model -> Html Msg
investigatingView conclusions subject { players, guessHistory, revealHistory } =
    case subject of
        PlayerHand playerId ->
            playerView conclusions playerId players

        People ->
            renderGameBoard Clue.personCards conclusions players

        Weapons ->
            renderGameBoard Clue.weaponCards conclusions players

        Rooms ->
            renderGameBoard Clue.roomCards conclusions players


renderMainDisplay : Model -> Html Msg
renderMainDisplay model =
    let
        conclusions =
            Conclusions.from model.players model.guessHistory model.revealHistory
    in
    case model.gameState of
        Investigating subject ->
            investigatingView conclusions subject model

        Guessing _ ->
            guessingForm conclusions model

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
