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


type alias Murder =
  { person: Person
  , weapon: Weapon
  , room: Room
  }

type alias Guess =
  { player: Player
  , murder: Murder
  }




---- MODEL ----


type alias Model =
  { players : List Player }

init : ( Model, Cmd Msg )
init =
  (
  { players = []
  }
  ,
  Cmd.none
  )



---- UPDATE ----


type Msg
    = SetPlayers Int
    | ResetGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SetPlayers playerCount ->
      ({ model | players = List.take playerCount possiblePlayers }, Cmd.none)
    ResetGame ->
      ({ model | players = [] }, Cmd.none)



---- VIEW ----

blankCell : Html msg
blankCell = td [] []

blankRow : List Player -> Html msg
blankRow players =
  tr [] (blankCell :: (List.map (\_ -> blankCell) players))

headerRow : List Player -> Html msg
headerRow players =
  tr [] (blankCell :: (List.map playerColumnHeader players))

playerColumnHeader : Player -> Html msg
playerColumnHeader player =
  th [] [ text (displayPlayer player) ]



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

selectNumberOfPlayers : Model -> Html Msg
selectNumberOfPlayers model =
  case model.players of
    [] -> div [] (List.map playerCountButton possibleNumbersOfPlayers)
    _ -> div [] []


resetGame : Model -> Html Msg
resetGame model =
  case model.players of
    [] -> div [] []
    _ -> button [ onClick ResetGame ] [ text "Reset" ]


gameBoard : Model -> Html Msg
gameBoard model =
  case model.players of
  [] -> div [] []
  _ -> table [ class "table" ]
    ( [ headerRow model.players ]
    ++ List.map (cardRow model.players) people
    ++ [ blankRow model.players ]
    ++ List.map (cardRow model.players) weapons
    ++ [ blankRow model.players ]
    ++ List.map (cardRow model.players) rooms
    )






view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ title
        , selectNumberOfPlayers model
        , resetGame model
        , gameBoard model
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
