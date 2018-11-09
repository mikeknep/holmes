module Domain exposing
    ( Card(..)
    , CompleteGuess
    , InProgressGuess(..)
    , Person(..)
    , Room(..)
    , Weapon(..)
    , allCards
    , personCards
    , roomCards
    , weaponCards
    )

import Player exposing (PlayerId)


type alias CompleteGuess =
    { guesser : PlayerId
    , person : Person
    , weapon : Weapon
    , room : Room
    , noShows : List PlayerId
    , shower : Maybe PlayerId
    }


type InProgressGuess
    = NothingIsSet
    | PersonIsSet Person
    | WeaponIsSet Person Weapon


type Person
    = MrGreen
    | ProfessorPlum
    | MissScarlet
    | ColMustard
    | MrsWhite
    | MrsPeacock


type Weapon
    = Knife
    | Rope
    | Candlestick
    | Pipe
    | Revolver
    | Wrench


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


type Card
    = PersonTag Person
    | WeaponTag Weapon
    | RoomTag Room


people : List Person
people =
    [ MrGreen, ProfessorPlum, MissScarlet, ColMustard, MrsWhite, MrsPeacock ]


personCards : List Card
personCards =
    List.map (\person -> PersonTag person) people


weapons : List Weapon
weapons =
    [ Knife, Rope, Candlestick, Pipe, Revolver, Wrench ]


weaponCards : List Card
weaponCards =
    List.map (\weapon -> WeaponTag weapon) weapons


rooms : List Room
rooms =
    [ Hall, Study, Conservatory, Kitchen, Ballroom, Lounge, Billiards, Library, Dining ]


roomCards : List Card
roomCards =
    List.map (\room -> RoomTag room) rooms


allCards =
    personCards ++ weaponCards ++ roomCards
