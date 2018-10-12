module CardPresenter exposing (displayCard)

import Domain exposing (..)


displayCard : Card -> String
displayCard card =
    case card of
        PersonTag person ->
            displayPerson person

        WeaponTag weapon ->
            displayWeapon weapon

        RoomTag room ->
            displayRoom room


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
