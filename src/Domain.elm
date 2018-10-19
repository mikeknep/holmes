module Domain exposing
    ( Card(..)
    , CompleteGuess
    , InProgressGuess(..)
    , Person(..)
    , Player
    , Room(..)
    , Weapon(..)
    , allCards
    , createPlayer
    , personCards
    , roomCards
    , weaponCards
    )


type alias CompleteGuess =
    { guesser : Player
    , person : Person
    , weapon : Weapon
    , room : Room
    , noShows : List Player
    , shower : Maybe Player
    }


type InProgressGuess
    = NothingIsSet
    | PersonIsSet Person
    | WeaponIsSet Person Weapon


type alias Player =
    { name : String
    }


createPlayer : String -> Player
createPlayer name =
    { name = name
    }


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
