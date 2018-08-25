module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)



---- DOMAIN ---

type Player = P1 | P2 | P3 | P4 | P5 | P6
possiblePlayers : List Player
possiblePlayers = [P1, P2, P3, P4, P5, P6]
possibleNumbersOfPlayers = [3,4,5,6]
displayPlayer : Player -> String
displayPlayer player =
  case player of
    P1 -> "P1"
    P2 -> "P2"
    P3 -> "P3"
    P4 -> "P4"
    P5 -> "P5"
    P6 -> "P6"

type Person = MrGreen | ProfessorPlum | MissScarlet | ColMustard | MrsWhite | MrsPeacock
people : List Card
people = [PersonTag MrGreen, PersonTag ProfessorPlum, PersonTag MissScarlet, PersonTag ColMustard, PersonTag MrsWhite, PersonTag MrsPeacock]
displayPerson : Person -> String
displayPerson person =
  case person of
    MrGreen -> "Mr. Green"
    ProfessorPlum -> "Professor Plum"
    MissScarlet -> "MissScarlet"
    ColMustard -> "Col. Mustard"
    MrsWhite -> "Mrs. White"
    MrsPeacock -> "Mrs. Peacock"

type Weapon = Knife | Rope | Candlestick | Pipe | Revolver | Wrench
weapons : List Card
weapons = [WeaponTag Knife, WeaponTag Rope, WeaponTag Candlestick, WeaponTag Pipe, WeaponTag Revolver, WeaponTag Wrench]
displayWeapon : Weapon -> String
displayWeapon weapon =
  case weapon of
    Knife -> "Knife"
    Rope -> "Rope"
    Candlestick -> "Candlestick"
    Pipe -> "Pipe"
    Revolver -> "Revolver"
    Wrench -> "Wrench"

type Room = Hall | Study | Conservatory | Kitchen | Ballroom | Lounge | Billiards | Library | Dining
rooms : List Card
rooms = [RoomTag Hall, RoomTag Study, RoomTag Conservatory, RoomTag Kitchen, RoomTag Ballroom, RoomTag Lounge, RoomTag Billiards, RoomTag Library, RoomTag Dining]
displayRoom : Room -> String
displayRoom room =
  case room of
    Hall -> "Hall"
    Study -> "Study"
    Conservatory -> "Conservatory"
    Kitchen -> "Kitchen"
    Ballroom -> "Ballroom"
    Lounge -> "Lounge"
    Billiards -> "Billiards Room"
    Library -> "Library"
    Dining -> "Dining Room"

type Card = PersonTag Person | WeaponTag Weapon | RoomTag Room
displayCard : Card -> String
displayCard card =
  case card of
    PersonTag person -> displayPerson person
    WeaponTag weapon -> displayWeapon weapon
    RoomTag room -> displayRoom room


type alias Guess =
  { player : Player
  , person : Maybe Person
  , weapon : Maybe Weapon
  , room : Maybe Room
  }



isJust : Maybe a -> Bool
isJust m =
  case m of
    Just _ -> True
    Nothing -> False

guessInProgress : Maybe Guess -> Bool
guessInProgress maybeGuess =
  isJust maybeGuess

guessIsComplete : Maybe Guess -> Bool
guessIsComplete maybeGuess =
  case maybeGuess of
    Nothing -> False
    Just guess -> isJust guess.person && isJust guess.weapon && isJust guess.room



---- MODEL ----

type DisplayMode = Board | Guessing


type alias Model =
  { players : List Player
  , guess : Maybe Guess
  , displaying : DisplayMode
  }

init : ( Model, Cmd Msg )
init =
  (
  { players = []
  , guess = Nothing
  , displaying = Board
  }
  ,
  Cmd.none
  )



---- UPDATE ----


type Msg
    = SetPlayers Int
    | ResetGame
    | BeginGuess Player
    | Display DisplayMode


beginGuess : Model -> Player -> ( Model, Cmd Msg )
beginGuess model player =
  case model.guess of
    Nothing -> ({ model | guess = Just { player = player, person = Nothing, weapon = Nothing, room = Nothing}}, Cmd.none)
    _ -> (model, Cmd.none)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SetPlayers playerCount ->
      ({ model | players = List.take playerCount possiblePlayers }, Cmd.none)
    ResetGame ->
      ({ model | players = [] }, Cmd.none)
    BeginGuess player ->
      beginGuess model player
    Display displayMode ->
      ({ model | displaying = displayMode }, Cmd.none)



---- VIEW ----

blankCell : Html msg
blankCell = td [] []

blankRow : List Player -> Html msg
blankRow players =
  tr [] (blankCell :: (List.map (\_ -> blankCell) players))

headerRow : List Player -> Html Msg
headerRow players =
  tr [] (blankCell :: (List.map playerColumnHeader players))

playerColumnHeader : Player -> Html Msg
playerColumnHeader player =
  th [ onClick (BeginGuess player) ] [ text (displayPlayer player) ]



cardPlayerCell : Card -> Player -> Html msg
cardPlayerCell card player =
  td [] [ text ((displayCard card) ++ "--" ++ (displayPlayer player)) ]

cardCell : Card -> Html msg
cardCell card =
  td [] [ text (displayCard card) ]

cardRow : List Player -> Card -> Html msg
cardRow players card =
  tr [] ((cardCell card) :: (List.map (cardPlayerCell card) players))



title : Html msg
title =
  h1 [] [ text "Clue!" ]


playerCountButton : Int -> Html Msg
playerCountButton numberOfPlayers =
  button [ onClick (SetPlayers numberOfPlayers) ] [ text (toString numberOfPlayers) ]

selectNumberOfPlayers : Html Msg
selectNumberOfPlayers =
    div [] (
      (h3 [] [text "Select number of players"])
      :: (List.map playerCountButton possibleNumbersOfPlayers)
      )


resetGame : Html Msg
resetGame =
  button [ onClick ResetGame ] [ text "Reset" ]


toggleBoardView : Model -> Html Msg
toggleBoardView model =
  case model.displaying of
    Board -> a [ class "button is-active" ] [ text "Board" ]
    _ -> a [ class "button", onClick (Display Board) ] [ text "Board" ]


toggleGuessView : Model -> Html Msg
toggleGuessView model =
  case model.displaying of
    Guessing -> a [ class "button is-active" ] [ text "Guess" ]
    _ -> a [ class "button", onClick (Display Guessing) ] [ text "Guess" ]


viewsAndActions : Model -> Html Msg
viewsAndActions model =
  case model.players of
    [] -> selectNumberOfPlayers
    _ -> div []
      [ toggleBoardView model
      , toggleGuessView model
      ]


gameBoard : Model -> Html Msg
gameBoard model =
  case model.displaying of
    Board -> table [ class "table" ]
      ( [ headerRow model.players ]
      ++ List.map (cardRow model.players) people
      ++ [ blankRow model.players ]
      ++ List.map (cardRow model.players) weapons
      ++ [ blankRow model.players ]
      ++ List.map (cardRow model.players) rooms
      )
    _ -> div [] []



renderMainDisplay : Model -> Html Msg
renderMainDisplay model =
  case model.displaying of
    Board -> gameBoard model
    Guessing -> div [] [p [] [text "Guessing mode"]]


mainDisplay : Model -> Html Msg
mainDisplay model =
  case model.players of
    [] -> div [] []
    _ -> renderMainDisplay model




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
