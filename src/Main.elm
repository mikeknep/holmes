module Main exposing (main)

import Browser exposing (element)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra exposing (takeWhile, takeWhileRight)



---- DOMAIN ---


type HoldingStatus
    = NotHolding
    | MaybeHolding Int
    | Holding


type alias Player =
    { name : String
    , cardholdingStatuses : Dict String HoldingStatus
    }


openingCardholdingStatuses : Dict String HoldingStatus
openingCardholdingStatuses =
    let
        allCards =
            personCards ++ weaponCards ++ roomCards
    in
    allCards
        |> List.map Debug.toString
        |> List.map (\card -> ( card, MaybeHolding 0 ))
        |> Dict.fromList


createPlayer : String -> Player
createPlayer name =
    { name = name
    , cardholdingStatuses = openingCardholdingStatuses
    }


possiblePlayers : List Player
possiblePlayers =
    List.map createPlayer [ "P1", "P2", "P3", "P4", "P5", "P6" ]


possibleNumbersOfPlayers =
    [ 3, 4, 5, 6 ]


type Person
    = MrGreen
    | ProfessorPlum
    | MissScarlet
    | ColMustard
    | MrsWhite
    | MrsPeacock


people : List Person
people =
    [ MrGreen, ProfessorPlum, MissScarlet, ColMustard, MrsWhite, MrsPeacock ]


personCards : List Card
personCards =
    List.map (\person -> PersonTag person) people


displayPerson : Person -> String
displayPerson person =
    case person of
        MrGreen ->
            "Mr. Green"

        ProfessorPlum ->
            "Professor Plum"

        MissScarlet ->
            "MissScarlet"

        ColMustard ->
            "Col. Mustard"

        MrsWhite ->
            "Mrs. White"

        MrsPeacock ->
            "Mrs. Peacock"


type Weapon
    = Knife
    | Rope
    | Candlestick
    | Pipe
    | Revolver
    | Wrench


weapons : List Weapon
weapons =
    [ Knife, Rope, Candlestick, Pipe, Revolver, Wrench ]


weaponCards : List Card
weaponCards =
    List.map (\weapon -> WeaponTag weapon) weapons


displayWeapon : Weapon -> String
displayWeapon weapon =
    case weapon of
        Knife ->
            "Knife"

        Rope ->
            "Rope"

        Candlestick ->
            "Candlestick"

        Pipe ->
            "Pipe"

        Revolver ->
            "Revolver"

        Wrench ->
            "Wrench"


type Room
    = Hall
    | Study
    | Conservatory
    | Kitchen
    | Ballroom
    | Lounge
    | Billiards
    | Library
    | Dining


rooms : List Room
rooms =
    [ Hall, Study, Conservatory, Kitchen, Ballroom, Lounge, Billiards, Library, Dining ]


roomCards : List Card
roomCards =
    List.map (\room -> RoomTag room) rooms


displayRoom : Room -> String
displayRoom room =
    case room of
        Hall ->
            "Hall"

        Study ->
            "Study"

        Conservatory ->
            "Conservatory"

        Kitchen ->
            "Kitchen"

        Ballroom ->
            "Ballroom"

        Lounge ->
            "Lounge"

        Billiards ->
            "Billiards Room"

        Library ->
            "Library"

        Dining ->
            "Dining Room"


type Card
    = PersonTag Person
    | WeaponTag Weapon
    | RoomTag Room


displayCard : Card -> String
displayCard card =
    case card of
        PersonTag person ->
            displayPerson person

        WeaponTag weapon ->
            displayWeapon weapon

        RoomTag room ->
            displayRoom room


type alias CompleteGuess =
    { guesser : Player
    , person : Person
    , weapon : Weapon
    , room : Room
    , noShows : List Player
    }


type InProgressGuess
    = NothingIsSet
    | PersonIsSet Person
    | WeaponIsSet Person Weapon



---- MODEL ----


type SubjectOfInvestigation
    = PlayerHand Player
    | People
    | Weapons
    | Rooms


type GameState
    = Guessing Player InProgressGuess
    | Revealing CompleteGuess
    | Investigating SubjectOfInvestigation


type alias Model =
    { players : List Player
    , gameState : GameState
    }


init : ( Model, Cmd Msg )
init =
    ( { players = []
      , gameState = Investigating People
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


updatePlayer : Player -> Player -> Player -> Player
updatePlayer updated original this =
    if original == this then
        updated

    else
        this


playerHasCard : Model -> Player -> Card -> ( Model, Cmd Msg )
playerHasCard model player card =
    let
        updatedCardholdingStatuses =
            Dict.update (Debug.toString card) (\_ -> Just Holding) player.cardholdingStatuses

        updatedPlayer =
            { player | cardholdingStatuses = updatedCardholdingStatuses }

        updatedPlayers =
            List.map (updatePlayer updatedPlayer player) model.players

        nextGameState =
            Investigating (PlayerHand updatedPlayer)
    in
    ( { players = updatedPlayers
      , gameState = nextGameState
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPlayers playerCount ->
            ( { model | players = List.take playerCount possiblePlayers }, Cmd.none )

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



---- VIEW ----


blankCell : Html msg
blankCell =
    td [] []


blankRow : List Player -> Html msg
blankRow players =
    tr [] (blankCell :: List.map (\_ -> blankCell) players)


headerRow : List Player -> Html Msg
headerRow players =
    tr [] (blankCell :: List.map playerColumnHeader players)


playerColumnHeader : Player -> Html Msg
playerColumnHeader player =
    th [] [ text player.name ]


cardPlayerCell : Card -> Player -> Html msg
cardPlayerCell card player =
    td [] [ text (displayPlayerHoldingStatus player card) ]


cardCell : Card -> Html msg
cardCell card =
    td [] [ text (displayCard card) ]


cardRow : List Player -> Card -> Html msg
cardRow players card =
    tr [] (cardCell card :: List.map (cardPlayerCell card) players)


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


gameBoard : List Card -> Model -> Html Msg
gameBoard cards model =
    div []
        [ table [ class "table" ]
            ([ headerRow model.players ]
                ++ List.map (cardRow model.players) cards
            )
        ]


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
    a [ class "button", onClick clickHandler ] [ text (displayCard card) ]


selectCard : List Card -> Html Msg
selectCard cards =
    div [] (List.map cardOption cards)


showerOption : Player -> List (Html msg)
showerOption player =
    [ dt [] [ text player.name ]
    , dd []
        [ button [] [ text "yes" ]
        , button [] [ text "no" ]
        ]
    ]



-- yes/no checkboxes
-- no :: update player's status for those three cards
-- no :: *last* player to say no should exit the page (avoid a separate 'nobody showed a card' button)
-- yes :: update player's status for those three cards, clears guess, returns to board view


displayGuess : CompleteGuess -> String
displayGuess guess =
    guess.guesser.name
        ++ " :: "
        ++ displayPerson guess.person
        ++ " :: "
        ++ displayWeapon guess.weapon
        ++ " :: "
        ++ displayRoom guess.room


otherPlayers : Player -> List Player -> List Player
otherPlayers guesser allPlayers =
    let
        isNotGuesser =
            \player -> player /= guesser
    in
    takeWhileRight isNotGuesser allPlayers ++ takeWhile isNotGuesser allPlayers


renderShowerOptions : CompleteGuess -> List Player -> Html msg
renderShowerOptions guess players =
    div []
        [ h3 [] [ text (displayGuess guess) ]
        , dl [] (List.concatMap showerOption (otherPlayers guess.guesser players))
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


displayPlayerHoldingStatus : Player -> Card -> String
displayPlayerHoldingStatus player card =
    let
        cardKey =
            Debug.toString card

        holdingStatus =
            Dict.get cardKey player.cardholdingStatuses
    in
    displayHoldingStatus holdingStatus


playerCardStatusAsDataListEntry : Player -> Card -> List (Html Msg)
playerCardStatusAsDataListEntry player card =
    [ dt [] [ text (displayCard card) ]
    , dd []
        [ text (displayPlayerHoldingStatus player card)
        , button [ onClick (PlayerHasCard player card) ] [ text "reveal" ]
        ]
    ]


playerView : Player -> Model -> Html Msg
playerView player model =
    div []
        [ h1 [] [ text player.name ]
        , p [] [ a [ class "button", onClick (BeginGuess player) ] [ text "Begin guess" ] ]
        , dl [] (List.concatMap (playerCardStatusAsDataListEntry player) personCards)
        , dl [] (List.concatMap (playerCardStatusAsDataListEntry player) weaponCards)
        , dl [] (List.concatMap (playerCardStatusAsDataListEntry player) roomCards)
        ]


investigatingView : SubjectOfInvestigation -> Model -> Html Msg
investigatingView subject model =
    case subject of
        PlayerHand player ->
            playerView player model

        People ->
            gameBoard personCards model

        Weapons ->
            gameBoard weaponCards model

        Rooms ->
            gameBoard roomCards model


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


selectPlayer : Player -> Html Msg
selectPlayer player =
    a [ class "button", onClick (Investigate (PlayerHand player)) ] [ text player.name ]


playerSelect : Model -> Html Msg
playerSelect model =
    div [] (List.map selectPlayer model.players)


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ title
        , setupNewGame model
        , playerSelect model
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
