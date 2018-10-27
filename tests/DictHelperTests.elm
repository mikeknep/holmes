module DictHelperTests exposing (all)

import Dict exposing (..)
import DictHelper exposing (..)
import Expect
import Test exposing (..)


all : Test
all =
    describe "Dict helpers"
        [ test "updateWhere updates values of keys matching a predicate" <|
            \_ ->
                let
                    fullDict =
                        Dict.fromList
                            [ ( 1, True )
                            , ( 2, True )
                            , ( 3, True )
                            , ( 4, True )
                            ]

                    whereClause =
                        \n _ -> modBy 2 n == 0

                    updateFn =
                        \_ _ -> False

                    expectedDict =
                        Dict.fromList
                            [ ( 1, True )
                            , ( 2, False )
                            , ( 3, True )
                            , ( 4, False )
                            ]
                in
                Expect.equal expectedDict (updateWhere whereClause updateFn fullDict)
        , test "any returns whether any k/v pair matching some predicate is present in the dict" <|
            \_ ->
                let
                    dict =
                        Dict.fromList
                            [ ( 1, True )
                            , ( 2, True )
                            , ( 3, True )
                            , ( 4, False )
                            ]

                    oneKeyMatches =
                        \k _ -> k == 1

                    oneValMatches =
                        \_ v -> v == False

                    nothingMatches =
                        \k v -> k == 4 && v == True
                in
                dict
                    |> Expect.all
                        [ \d -> Expect.equal True (any oneKeyMatches d)
                        , \d -> Expect.equal True (any oneValMatches d)
                        , \d -> Expect.equal False (any nothingMatches d)
                        ]
        ]
