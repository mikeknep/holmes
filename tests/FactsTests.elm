module FactsTests exposing (all)

import Dict exposing (fromList)
import Domain exposing (..)
import Expect
import Facts exposing (HoldingStatus(..), analyze)
import Test exposing (..)


all : Test
all =
    describe "Facts analysis"
        [ test """
       When a player reveals a card,
       all other players are updated to not holding that card.
       """ <|
            \_ ->
                let
                    initialFacts =
                        Dict.fromList
                            [ ( ( "MrGreen", 1 ), MaybeHolding 0 )
                            , ( ( "MrGreen", 2 ), MaybeHolding 0 )
                            , ( ( "MrGreen", 3 ), Holding )
                            , ( ( "Wrench", 1 ), NotHolding )
                            , ( ( "Wrench", 2 ), MaybeHolding 0 )
                            , ( ( "Wrench", 3 ), NotHolding )
                            ]

                    ( playerIds, history ) =
                        ( [ 1, 2, 3 ], [] )

                    expectedFacts =
                        Dict.fromList
                            [ ( ( "MrGreen", 1 ), NotHolding )
                            , ( ( "MrGreen", 2 ), NotHolding )
                            , ( ( "MrGreen", 3 ), Holding )
                            , ( ( "Wrench", 1 ), NotHolding )
                            , ( ( "Wrench", 2 ), MaybeHolding 0 )
                            , ( ( "Wrench", 3 ), NotHolding )
                            ]
                in
                Expect.equal expectedFacts (analyze playerIds history initialFacts)
        , test """
        If a player reveals some card for a guess,
        and later is known to not have two of those cards,
        then we know they must be holding the third card from that guess.
        """ <|
            \_ ->
                let
                    initialFacts =
                        Dict.fromList
                            [ ( ( "MissScarlet", 1 ), NotHolding )
                            , ( ( "Revolver", 1 ), NotHolding )
                            , ( ( "Conservatory", 1 ), MaybeHolding 0 )
                            ]

                    history =
                        [ { guesser = 4
                          , person = MissScarlet
                          , weapon = Revolver
                          , room = Conservatory
                          , noShows = []
                          , shower = Just 1
                          }
                        ]

                    expectedFacts =
                        Dict.fromList
                            [ ( ( "MissScarlet", 1 ), NotHolding )
                            , ( ( "Revolver", 1 ), NotHolding )
                            , ( ( "Conservatory", 1 ), Holding )
                            ]
                in
                Expect.equal expectedFacts (analyze [ 1, 2, 3 ] history initialFacts)
        , test """
       If the game has three players,
       and a certain player is known to have four cards,
       then we know that player has no other cards.
       """ <|
            \_ ->
                let
                    initialFacts =
                        Dict.fromList
                            [ ( ( "MrsWhite", 1 ), Holding )
                            , ( ( "Candlestick", 1 ), Holding )
                            , ( ( "Hall", 1 ), Holding )
                            , ( ( "BilliardsRoom", 1 ), Holding )
                            , ( ( "Kitchen", 1 ), MaybeHolding 0 )
                            , ( ( "Ballroom", 1 ), MaybeHolding 0 )
                            ]

                    ( playerIds, history ) =
                        ( [ 1, 2, 3 ], [] )

                    expectedFacts =
                        Dict.fromList
                            [ ( ( "MrsWhite", 1 ), Holding )
                            , ( ( "Candlestick", 1 ), Holding )
                            , ( ( "Hall", 1 ), Holding )
                            , ( ( "BilliardsRoom", 1 ), Holding )
                            , ( ( "Kitchen", 1 ), NotHolding )
                            , ( ( "Ballroom", 1 ), NotHolding )
                            ]
                in
                Expect.equal expectedFacts (analyze playerIds history initialFacts)
        , test """
       If the game has six players,
       and a certain player is known to have three cards,
       then we know that player has no other cards.
       """ <|
            \_ ->
                let
                    initialFacts =
                        Dict.fromList
                            [ ( ( "MrsWhite", 1 ), Holding )
                            , ( ( "Candlestick", 1 ), Holding )
                            , ( ( "Hall", 1 ), Holding )
                            , ( ( "BilliardsRoom", 1 ), MaybeHolding 0 )
                            , ( ( "Kitchen", 1 ), MaybeHolding 0 )
                            , ( ( "Ballroom", 1 ), MaybeHolding 0 )
                            ]

                    ( playerIds, history ) =
                        ( [ 1, 2, 3, 4, 5, 6 ], [] )

                    expectedFacts =
                        Dict.fromList
                            [ ( ( "MrsWhite", 1 ), Holding )
                            , ( ( "Candlestick", 1 ), Holding )
                            , ( ( "Hall", 1 ), Holding )
                            , ( ( "BilliardsRoom", 1 ), NotHolding )
                            , ( ( "Kitchen", 1 ), NotHolding )
                            , ( ( "Ballroom", 1 ), NotHolding )
                            ]
                in
                Expect.equal expectedFacts (analyze playerIds history initialFacts)
        , test """
       "Integration" test:
       A player reveals a card,
       setting all other players to not holding that card,
       and updating a different player to be holding a different card based on guess history.
       """ <|
            \_ ->
                let
                    initialFacts =
                        Dict.fromList
                            [ ( ( "MrsWhite", 1 ), MaybeHolding 0 )
                            , ( ( "Candlestick", 1 ), NotHolding )
                            , ( ( "Hall", 1 ), MaybeHolding 0 )
                            , ( ( "MrsWhite", 2 ), Holding )
                            , ( ( "MrsWhite", 3 ), MaybeHolding 0 )
                            ]

                    playerIds =
                        [ 1, 2, 3, 4, 5, 6 ]

                    history =
                        [ { guesser = 4
                          , person = MrsWhite
                          , weapon = Candlestick
                          , room = Hall
                          , noShows = []
                          , shower = Just 1
                          }
                        ]

                    expectedFacts =
                        Dict.fromList
                            [ ( ( "MrsWhite", 1 ), NotHolding )
                            , ( ( "Candlestick", 1 ), NotHolding )
                            , ( ( "Hall", 1 ), Holding )
                            , ( ( "MrsWhite", 2 ), Holding )
                            , ( ( "MrsWhite", 3 ), NotHolding )
                            ]
                in
                Expect.equal expectedFacts (analyze playerIds history initialFacts)
        ]
