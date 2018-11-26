module Clue exposing
    ( Card
    , CardId
    , CompleteGuess
    , GuessHistory
    , IncompleteGuess
    , Reveal
    , RevealHistory
    , addCardToGuess
    , addGuessToHistory
    , addNoShowToGuess
    , addRevealToHistory
    , addShowerToGuess
    , allCards
    , allGuesses
    , allReveals
    , beginGuess
    , createReveal
    , displayCard
    , displayCardWithId
    , finishGuess
    , getCardId
    , getCardIdsFromGuess
    , getCardOptionsForGuess
    , getGuesser
    , getNoShows
    , getRevealedCard
    , getRevealer
    , getShower
    , noGuesses
    , noReveals
    , personCards
    , roomCards
    , weaponCards
    )

import Player exposing (PlayerId)


type alias CardId =
    String


getCardId : Card -> CardId
getCardId (Card { id }) =
    id


beginGuess : PlayerId -> IncompleteGuess
beginGuess playerId =
    IncompleteGuess
        { guesser = playerId
        , person = Nothing
        , weapon = Nothing
        , room = Nothing
        , noShows = []
        , shower = Nothing
        }


type Card
    = Card CardDetails


type alias CardDetails =
    { displayName : String
    , id : CardId
    }


displayCard : Card -> String
displayCard (Card { displayName }) =
    displayName


getCardWithId : CardId -> Maybe Card
getCardWithId cardId =
    allCards
        |> List.filter (\card -> getCardId card == cardId)
        |> List.head


displayCardWithId : CardId -> String
displayCardWithId cardId =
    getCardWithId cardId |> Maybe.map displayCard |> Maybe.withDefault ""


personCards : List Card
personCards =
    List.map Card
        [ CardDetails "Col. Mustard" "mustard"
        , CardDetails "Prof. Plum" "plum"
        , CardDetails "Mr. Green" "green"
        , CardDetails "Ms. Peacock" "peacock"
        , CardDetails "Miss Scarlet" "scarlet"
        , CardDetails "Mrs. White" "white"
        ]


weaponCards : List Card
weaponCards =
    List.map Card
        [ CardDetails "Candlestick" "candlestick"
        , CardDetails "Knife" "knife"
        , CardDetails "Lead pipe" "pipe"
        , CardDetails "Revolver" "revolver"
        , CardDetails "Rope" "rope"
        , CardDetails "Wrench" "wrench"
        ]


roomCards : List Card
roomCards =
    List.map Card
        [ CardDetails "Ballroom" "ballroom"
        , CardDetails "Billiard Room" "billiard"
        , CardDetails "Conservatory" "conservatory"
        , CardDetails "Dining Room" "dining"
        , CardDetails "Hall" "hall"
        , CardDetails "Kitchen" "kitchen"
        , CardDetails "Library" "library"
        , CardDetails "Lounge" "lounge"
        , CardDetails "Study" "study"
        ]


allCards : List Card
allCards =
    personCards ++ weaponCards ++ roomCards


groupContainsCard : CardId -> List Card -> Bool
groupContainsCard cardId cards =
    cards
        |> List.map getCardId
        |> List.member cardId


addCardToGuess : CardId -> IncompleteGuess -> IncompleteGuess
addCardToGuess cardId (IncompleteGuess details) =
    let
        groupContainsThisCard =
            groupContainsCard cardId
    in
    if groupContainsThisCard personCards then
        IncompleteGuess
            { details | person = Just cardId }

    else if groupContainsThisCard weaponCards then
        IncompleteGuess
            { details | weapon = Just cardId }

    else if groupContainsThisCard roomCards then
        IncompleteGuess
            { details | room = Just cardId }

    else
        IncompleteGuess details


type alias BaseGuessDetails a =
    { a
        | guesser : PlayerId
        , noShows : List PlayerId
        , shower : Maybe PlayerId
    }


type alias MaybeSetCards =
    { person : Maybe CardId, weapon : Maybe CardId, room : Maybe CardId }


type alias IncompleteGuessDetails =
    BaseGuessDetails MaybeSetCards


type alias DefinitelySetCards =
    { person : CardId, weapon : CardId, room : CardId }


type alias CompleteGuessDetails =
    BaseGuessDetails DefinitelySetCards


type IncompleteGuess
    = IncompleteGuess IncompleteGuessDetails


type CompleteGuess
    = CompleteGuess CompleteGuessDetails


finishGuess : IncompleteGuess -> Result IncompleteGuess CompleteGuess
finishGuess (IncompleteGuess details) =
    case ( details.person, details.weapon, details.room ) of
        ( Just person, Just weapon, Just room ) ->
            Ok
                (CompleteGuess
                    { shower = details.shower
                    , noShows = details.noShows
                    , guesser = details.guesser
                    , person = person
                    , weapon = weapon
                    , room = room
                    }
                )

        _ ->
            Err (IncompleteGuess details)


addNoShowToGuess : PlayerId -> CompleteGuess -> CompleteGuess
addNoShowToGuess playerId (CompleteGuess details) =
    CompleteGuess { details | noShows = playerId :: details.noShows }


getNoShows : CompleteGuess -> List PlayerId
getNoShows (CompleteGuess { noShows }) =
    noShows


addShowerToGuess : PlayerId -> CompleteGuess -> CompleteGuess
addShowerToGuess playerId (CompleteGuess details) =
    CompleteGuess { details | shower = Just playerId }


getGuesser : CompleteGuess -> PlayerId
getGuesser (CompleteGuess { guesser }) =
    guesser


getShower : CompleteGuess -> Maybe PlayerId
getShower (CompleteGuess { shower }) =
    shower


getCardIdsFromGuess : CompleteGuess -> List CardId
getCardIdsFromGuess (CompleteGuess { person, weapon, room }) =
    [ person, weapon, room ]


getCardOptionsForGuess : IncompleteGuess -> List Card
getCardOptionsForGuess (IncompleteGuess { person, weapon, room }) =
    case ( person, weapon, room ) of
        ( Nothing, _, _ ) ->
            personCards

        ( _, Nothing, _ ) ->
            weaponCards

        ( _, _, Nothing ) ->
            roomCards

        _ ->
            []


type GuessHistory
    = GuessHistory (List CompleteGuess)


noGuesses : GuessHistory
noGuesses =
    GuessHistory []


addGuessToHistory : CompleteGuess -> GuessHistory -> GuessHistory
addGuessToHistory guess (GuessHistory history) =
    GuessHistory (guess :: history)


allGuesses : GuessHistory -> List CompleteGuess
allGuesses (GuessHistory history) =
    history


type Reveal
    = Reveal RevealDetails


type alias RevealDetails =
    ( CardId, PlayerId )


createReveal : CardId -> PlayerId -> Reveal
createReveal cardId playerId =
    Reveal ( cardId, playerId )


getRevealedCard : Reveal -> CardId
getRevealedCard (Reveal ( cardId, _ )) =
    cardId


getRevealer : Reveal -> PlayerId
getRevealer (Reveal ( _, playerId )) =
    playerId


type RevealHistory
    = RevealHistory (List Reveal)


noReveals : RevealHistory
noReveals =
    RevealHistory []


addRevealToHistory : Reveal -> RevealHistory -> RevealHistory
addRevealToHistory reveal (RevealHistory history) =
    RevealHistory (reveal :: history)


allReveals : RevealHistory -> List Reveal
allReveals (RevealHistory history) =
    history
