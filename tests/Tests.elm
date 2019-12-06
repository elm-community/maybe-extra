module Tests exposing (suite)

import Array
import Expect
import Maybe.Extra exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Maybe.Extra"
        [ describe "orElse"
            [ test "both Just" <|
                \() ->
                    Just 4
                        |> orElse (Just 5)
                        |> Expect.equal (Just 4)
            , test "pipe input Nothing" <|
                \() ->
                    Nothing
                        |> orElse (Just 5)
                        |> Expect.equal (Just 5)
            , test "pipe function Nothing" <|
                \() ->
                    Just 4
                        |> orElse Nothing
                        |> Expect.equal (Just 4)
            , test "both Nothing" <|
                \() ->
                    Nothing
                        |> orElse Nothing
                        |> Expect.equal Nothing
            ]
        , describe "orLazy"
            [ test "both Just" <|
                \() ->
                    orLazy (Just 4) (\() -> Just 5)
                        |> Expect.equal (Just 4)
            , test "first Nothing" <|
                \() ->
                    orLazy Nothing (\() -> Just 5)
                        |> Expect.equal (Just 5)
            , test "second Nothing" <|
                \() ->
                    orLazy (Just 4) (\() -> Nothing)
                        |> Expect.equal (Just 4)
            , test "both Nothing" <|
                \() ->
                    orLazy Nothing (\() -> Nothing)
                        |> Expect.equal Nothing
            ]
        , describe "orElseLazy"
            [ test "both Just" <|
                \() ->
                    Just 4
                        |> orElseLazy (\() -> Just 5)
                        |> Expect.equal (Just 4)
            , test "pipe input Nothing" <|
                \() ->
                    Nothing
                        |> orElseLazy (\() -> Just 5)
                        |> Expect.equal (Just 5)
            , test "pipe function Nothing" <|
                \() ->
                    Just 4
                        |> orElseLazy (\() -> Nothing)
                        |> Expect.equal (Just 4)
            , test "both Nothing" <|
                \() ->
                    Nothing
                        |> orElseLazy (\() -> Nothing)
                        |> Expect.equal Nothing
            ]
        , describe "orListLazy"
            [ test "empty" <|
                \() ->
                    []
                        |> orListLazy
                        |> Expect.equal Nothing
            , test "all nothing" <|
                \() ->
                    [ \() -> Nothing
                    , \() -> List.head []
                    , \() -> String.toInt ""
                    ]
                        |> orListLazy
                        |> Expect.equal Nothing
            ]
        , describe "traverseArray"
            [ test "empty" <|
                \() ->
                    Array.empty
                        |> traverseArray (\x -> Just (x * 10))
                        |> Expect.equal (Just Array.empty)
            , test "all Just" <|
                \() ->
                    [ 1, 2, 3, 4, 5 ]
                        |> Array.fromList
                        |> traverseArray (\x -> Just (x * 10))
                        |> Expect.equal (Just (Array.fromList [ 10, 20, 30, 40, 50 ]))
            , test "one Nothing fails the whole function" <|
                \() ->
                    [ [ 1 ], [ 2, 3 ], [] ]
                        |> Array.fromList
                        |> traverseArray List.head
                        |> Expect.equal Nothing
            ]
        , describe "combineArray"
            [ test "empty" <|
                \() ->
                    Array.empty
                        |> combineArray
                        |> Expect.equal (Just Array.empty)
            , test "succeed" <|
                \() ->
                    [ Just 1, Just 2, Just 3 ]
                        |> Array.fromList
                        |> combineArray
                        |> Expect.equal (Just (Array.fromList [ 1, 2, 3 ]))
            , test "fail" <|
                \() ->
                    [ Just 1, Nothing ]
                        |> Array.fromList
                        |> combineArray
                        |> Expect.equal Nothing
            ]
        , describe "oneOf"
            [ test "empty" <|
                \() ->
                    oneOf [] 0
                        |> Expect.equal Nothing
            , test "all fail" <|
                \() ->
                    oneOf (List.repeat 10 (always Nothing)) 0
                        |> Expect.equal Nothing
            , test "last function succeeds" <|
                \() ->
                    oneOf [ always Nothing, always Nothing, always Nothing, always (Just True) ] 0
                        |> Expect.equal (Just True)
            , test "first function succeeds" <|
                \() ->
                    0
                        |> oneOf [ Just, Just << (+) 10, Just << (+) 20 ]
                        |> Expect.equal (Just 0)
            ]
        , describe "toMaybe"
            [ test "false" <|
                \() ->
                    toMaybe False 0
                        |> Expect.equal Nothing
            , test "true" <|
                \() ->
                    toMaybe True 0
                        |> Expect.equal (Just 0)
            ]
        , describe "guarded"
            [ test "Predicate fails" <|
                \() ->
                    guarded ((<) 10) 5
                        |> Expect.equal Nothing
            , test "Predicate succeeds" <|
                \() ->
                    guarded ((<) 2) 5
                        |> Expect.equal (Just 5)
            ]
        ]
