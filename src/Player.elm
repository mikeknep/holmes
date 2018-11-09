module Player exposing (Player, PlayerId, createPlayer, getId, getName)


type alias PlayerId =
    Int


type Player
    = Player PlayerDetails


type alias PlayerDetails =
    { id : PlayerId
    , name : String
    , numberOfCardsInHand : Int
    }


createPlayer : PlayerId -> String -> Player
createPlayer playerId name =
    Player
        { id = playerId
        , name = name
        , numberOfCardsInHand = 4
        }


getId : Player -> PlayerId
getId (Player { id }) =
    id


getName : Player -> String
getName (Player { name }) =
    name
