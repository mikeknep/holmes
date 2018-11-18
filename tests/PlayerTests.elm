module PlayerTests exposing (all)

import Expect
import Player exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Player"
        [ describe "adding a new player"
            [ test "a valid player can be added" <|
                \_ ->
                    let
                        names =
                            Player.noPlayers
                                |> Player.addNewPlayer "Mike"
                                |> Player.allPlayers
                                |> List.map Player.getName
                    in
                    Expect.equal [ "Mike" ] names
            , test "an empty string is not a valid player name" <|
                \_ ->
                    let
                        names =
                            Player.noPlayers
                                |> Player.addNewPlayer ""
                                |> Player.allPlayers
                    in
                    Expect.equal [] names
            ]
        ]
