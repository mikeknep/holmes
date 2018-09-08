module Main exposing (main)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)



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
        |> List.map toString
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


type alias Guess =
    { player : Player
    , person : Maybe Person
    , weapon : Maybe Weapon
    , room : Maybe Room
    }



---- MODEL ----


type DisplayMode
    = Board
    | Guessing


type alias Model =
    { players : List Player
    , guess : Maybe Guess
    , displaying : DisplayMode
    }


init : ( Model, Cmd Msg )
init =
    ( { players = []
      , guess = Nothing
      , displaying = Board
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SetPlayers Int
    | ResetGame
    | BeginGuess Player
    | Display DisplayMode
    | SetCardGuess Card


newGuess : Player -> Guess
newGuess player =
    { player = player
    , person = Nothing
    , weapon = Nothing
    , room = Nothing
    }


beginGuess : Model -> Player -> ( Model, Cmd Msg )
beginGuess model player =
    case model.guess of
        Nothing ->
            ( { model | guess = Just (newGuess player) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


attachCardToGuess : Card -> Guess -> Guess
attachCardToGuess card guess =
    case card of
        PersonTag person ->
            { guess | person = Just person }

        WeaponTag weapon ->
            { guess | weapon = Just weapon }

        RoomTag room ->
            { guess | room = Just room }


setCardGuess : Model -> Card -> ( Model, Cmd Msg )
setCardGuess model card =
    let
        newGuess =
            Maybe.map (attachCardToGuess card) model.guess
    in
    ( { model | guess = newGuess }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPlayers playerCount ->
            ( { model | players = List.take playerCount possiblePlayers }, Cmd.none )

        ResetGame ->
            init

        BeginGuess player ->
            beginGuess model player

        SetCardGuess card ->
            setCardGuess model card

        Display displayMode ->
            ( { model | displaying = displayMode }, Cmd.none )



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
    td [] [ text (displayCard card ++ "--" ++ player.name) ]


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
    button [ onClick (SetPlayers numberOfPlayers) ] [ text (toString numberOfPlayers) ]


selectNumberOfPlayers : Html Msg
selectNumberOfPlayers =
    div []
        (h3 [] [ text "Select number of players" ]
            :: List.map playerCountButton possibleNumbersOfPlayers
        )


resetGame : Html Msg
resetGame =
    button [ onClick ResetGame ] [ text "Reset" ]


toggleBoardView : Model -> Html Msg
toggleBoardView model =
    case model.displaying of
        Board ->
            a [ class "button is-active" ] [ text "Board" ]

        _ ->
            a [ class "button", onClick (Display Board) ] [ text "Board" ]


toggleGuessView : Model -> Html Msg
toggleGuessView model =
    case model.displaying of
        Guessing ->
            a [ class "button is-active" ] [ text "Guess" ]

        _ ->
            a [ class "button", onClick (Display Guessing) ] [ text "Guess" ]


viewsAndActions : Model -> Html Msg
viewsAndActions model =
    case model.players of
        [] ->
            selectNumberOfPlayers

        _ ->
            div []
                [ toggleBoardView model
                , toggleGuessView model
                ]


gameBoard : Model -> Html Msg
gameBoard model =
    case model.displaying of
        Board ->
            table [ class "table" ]
                ([ headerRow model.players ]
                    ++ List.map (cardRow model.players) personCards
                    ++ [ blankRow model.players ]
                    ++ List.map (cardRow model.players) weaponCards
                    ++ [ blankRow model.players ]
                    ++ List.map (cardRow model.players) roomCards
                )

        _ ->
            div [] []


guesserOption : Player -> Html Msg
guesserOption player =
    a [ class "button", onClick (BeginGuess player) ] [ text player.name ]


selectGuesser : List Player -> Html Msg
selectGuesser players =
    div []
        (h2 [] [ text "Who is guessing?" ]
            :: List.map guesserOption players
        )


cardOption : Card -> Html Msg
cardOption card =
    a [ class "button", onClick (SetCardGuess card) ] [ text (displayCard card) ]


selectCard : List Card -> Html Msg
selectCard cards =
    div [] (List.map cardOption cards)


showerOption : Player -> Html msg
showerOption player =
    -- yes/no checkboxes
    -- no :: update player's status for those three cards
    -- no :: *last* player to say no should exit the page (avoid a separate 'nobody showed a card' button)
    -- yes :: update player's status for those three cards, clears guess, returns to board view
    p [] [ text "Shower option" ]


displayGuess : Guess -> String
displayGuess guess =
    "Display the guess here"


renderShowerOptions : Guess -> List Player -> Html msg
renderShowerOptions guess players =
    let
        possibleShowers =
            List.filter (\player -> player /= guess.player) players
    in
    div [] ([ h3 [] [ text (displayGuess guess) ] ] ++ List.map showerOption possibleShowers)


selectCards : Guess -> List Player -> Html Msg
selectCards guess players =
    case guess.person of
        Nothing ->
            selectCard personCards

        Just _ ->
            case guess.weapon of
                Nothing ->
                    selectCard weaponCards

                Just _ ->
                    case guess.room of
                        Nothing ->
                            selectCard roomCards

                        Just _ ->
                            renderShowerOptions guess players


guessingForm : Model -> Html Msg
guessingForm model =
    case model.guess of
        Nothing ->
            selectGuesser model.players

        Just guess ->
            selectCards guess model.players


renderMainDisplay : Model -> Html Msg
renderMainDisplay model =
    case model.displaying of
        Board ->
            gameBoard model

        Guessing ->
            guessingForm model


mainDisplay : Model -> Html Msg
mainDisplay model =
    case model.players of
        [] ->
            div [] []

        _ ->
            renderMainDisplay model


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ title
        , viewsAndActions model
        , mainDisplay model
        , resetGame
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
