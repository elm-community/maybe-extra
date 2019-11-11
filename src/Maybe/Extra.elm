module Maybe.Extra exposing
    ( isJust, isNothing, join, filter
    , unwrap, unpack
    , or, orElse, orLazy, orElseLazy
    , values
    , combine, traverse, combineArray, traverseArray
    , toList, toArray
    , andMap, next, prev
    )

{-| Convenience functions for `Maybe`.


# Basics

Work with 1 `Maybe`

@docs isJust, isNothing, join, filter


# Get a value out of a `Maybe`

@docs unwrap, unpack


# Or: Combine 2 `Maybe`s

@docs or, orElse, orLazy, orElseLazy


# Lists of `Maybe`s

@docs values
@docs combine, traverse, combineArray, traverseArray


# toList

@docs toList, toArray


# Applicative Functions

@docs andMap, next, prev

-}

import Array
import Maybe exposing (..)



-- Basics: Work with 1 `Maybe`


{-| Conveniently check if a `Maybe` matches `Just _`.

    isJust (Just 42)
    --> True

    isJust (Just [])
    --> True

    isJust Nothing
    --> False

-}
isJust : Maybe a -> Bool
isJust m =
    case m of
        Nothing ->
            False

        Just _ ->
            True


{-| Conveniently check if a `Maybe` matches `Nothing`.

    isNothing (Just 42)
    --> False

    isNothing (Just [])
    --> False

    isNothing Nothing
    --> True

-}
isNothing : Maybe a -> Bool
isNothing m =
    case m of
        Nothing ->
            True

        Just _ ->
            False


{-| Flattens nested `Maybe`s

    join (Just (Just 1))
    --> Just 1

    join (Just Nothing)
    --> Nothing

    join Nothing
    --> Nothing

-}
join : Maybe (Maybe a) -> Maybe a
join mx =
    case mx of
        Just x ->
            x

        Nothing ->
            Nothing


{-| Take a `Maybe` and a predicate function and return a `Maybe` with the original value when a predicate matches.

    filter (\v -> v == 1) (Just 1)
    --> Just 1

    filter (\v -> v == 2) (Just 1)
    --> Nothing

-}
filter : (a -> Bool) -> Maybe a -> Maybe a
filter f m =
    case Maybe.map f m of
        Just True ->
            m

        _ ->
            Nothing



-- Get a value out of a `Maybe`


{-| Take a default value, a function and a `Maybe`.
Return the default value if the `Maybe` is `Nothing`.
If the `Maybe` is `Just a`, apply the function on `a` and return the `b`.
That is, `unwrap d f` is equivalent to `Maybe.map f >> Maybe.withDefault d`.

    unwrap 0 String.length Nothing
    --> 0

    unwrap 0 String.length (Just "abc")
    --> 3

-}
unwrap : b -> (a -> b) -> Maybe a -> b
unwrap d f m =
    case m of
        Nothing ->
            d

        Just a ->
            f a


{-| A version of `unwrap` that is non-strict in the default value (by
having it passed in a thunk).

    unpack (\() -> 0) String.length Nothing
    --> 0

    unpack (\() -> 0) String.length (Just "abc")
    --> 3

-}
unpack : (() -> b) -> (a -> b) -> Maybe a -> b
unpack d f m =
    case m of
        Nothing ->
            d ()

        Just a ->
            f a



-- Or: Combine 2 `Maybe`s


{-| Like the boolean `||` this will return the first value that is
positive (`Just`). However, unlike with `||`, both values will be
computed anyway (there is no short-circuiting).

    or (Just 4) (Just 5)
    --> Just 4

    or (Just 4) Nothing
    --> Just 4

    or Nothing (Just 5)
    --> Just 5

    or Nothing Nothing
    --> Nothing

Advanced functional programmers will recognize this as the
implementation of `mplus` for `Maybe`s from the `MonadPlus` type
class.

-}
or : Maybe a -> Maybe a -> Maybe a
or ma mb =
    case ma of
        Nothing ->
            mb

        Just _ ->
            ma


{-| Piping-friendly version of `or`.

    Just 5
        |> orElse (Just 4)
    --> Just 5

    orElse (Just 4) (Just 5)
    --> Just 5

    List.head []
        |> orElse (List.head [ 4 ])
    --> Just 4

-}
orElse : Maybe a -> Maybe a -> Maybe a
orElse ma mb =
    case mb of
        Nothing ->
            ma

        Just _ ->
            mb


{-| Lazy version of `or`.
The second argument will only be evaluated if the first argument is `Nothing`.

    orLazy (Just 4) (\() -> Just 5)
    --> Just 4

-}
orLazy : Maybe a -> (() -> Maybe a) -> Maybe a
orLazy ma fmb =
    case ma of
        Nothing ->
            fmb ()

        Just _ ->
            ma


{-| Lazy version of `orElse`.
The first argument will only be evaluated if the second argument is `Nothing`.
Piping-friendly.

    List.head []
        |> orElseLazy (\() -> List.head [ 4 ])
    --> Just 4

-}
orElseLazy : (() -> Maybe a) -> Maybe a -> Maybe a
orElseLazy fma mb =
    case mb of
        Nothing ->
            fma ()

        Just _ ->
            mb



-- Lists of `Maybe`s


{-| Convert a list of `Maybe a` to a list of `a` only for the values different
from `Nothing`.

    values [ Just 1, Nothing, Just 2 ]
    --> [ 1, 2 ]

-}
values : List (Maybe a) -> List a
values =
    List.foldr foldrValues []


foldrValues : Maybe a -> List a -> List a
foldrValues item list =
    case item of
        Nothing ->
            list

        Just v ->
            v :: list


{-| Take a list of `Maybe`s and return a `Maybe` with a list of values. `combine == traverse identity`.

    combine []
    --> Just []

    combine [ Just 1, Just 2, Just 3 ]
    --> Just [ 1, 2, 3 ]

    combine [ Just 1, Nothing, Just 3 ]
    --> Nothing

-}
combine : List (Maybe a) -> Maybe (List a)
combine =
    traverse identity


{-| Take a function that returns `Maybe` value and a list. Map a function over each element of the list. Collect the result in the list within `Maybe`.

    traverse (\x -> Just (x * 10)) [ 1, 2, 3, 4, 5 ]
    --> Just [ 10, 20, 30, 40, 50 ]

If any element returns Nothing, the whole function fails

    traverse List.head [ [1], [2, 3], [] ]
    --> Nothing

-}
traverse : (a -> Maybe b) -> List a -> Maybe (List b)
traverse f =
    let
        step e acc =
            case f e of
                Nothing ->
                    Nothing

                Just x ->
                    map ((::) x) acc
    in
    List.foldr step (Just [])


{-| -}
combineArray : Array.Array (Maybe a) -> Maybe (Array.Array a)
combineArray =
    traverseArray identity


{-| -}
traverseArray : (a -> Maybe b) -> Array.Array a -> Maybe (Array.Array b)
traverseArray f =
    let
        step e acc =
            case f e of
                Nothing ->
                    Nothing

                Just x ->
                    map (Array.push x) acc
    in
    Array.foldl step (Just Array.empty)



-- toList


{-| Return an empty list on `Nothing` or a list with one element, where the element is the value of `Just`.

    toList Nothing
    --> []

    toList (Just 1)
    --> [ 1 ]

-}
toList : Maybe a -> List a
toList m =
    case m of
        Nothing ->
            []

        Just x ->
            [ x ]


{-| Return an empty array on `Nothing` or a list with one element, where the element is the value of `Just`.

    import Array

    toArray Nothing
    --> Array.fromList []

    toArray (Just 1)
    --> Array.fromList [ 1 ]

-}
toArray : Maybe a -> Array.Array a
toArray m =
    case m of
        Nothing ->
            Array.empty

        Just x ->
            Array.repeat 1 x



-- Applicative Functions


{-| Apply the function that is inside `Maybe` to a value that is inside `Maybe`. Return the result inside `Maybe`. If one of the `Maybe` arguments is `Nothing`, return `Nothing`.

    Just ((+) 2) |> andMap (Just 3)
    --> Just 5

    Nothing |> andMap (Just 3)
    --> Nothing

    Just ((+) 2) |> andMap Nothing
    --> Nothing

Advanced functional programmers will recognize this as the implementation of `<*>` for `Maybe`s from the `Applicative` typeclass.

-}
andMap : Maybe a -> Maybe (a -> b) -> Maybe b
andMap =
    Maybe.map2 (|>)


{-| Take two `Maybe` values. If the first one equals `Nothing`, return `Nothing`. Otherwise return the second value.

    next (Just 1) (Just 2)
    --> Just 2

    next Nothing (Just 2)
    --> Nothing

    next (Just 1) Nothing
    --> Nothing

Advanced functional programmers will recognize this as the implementation of `*>` for `Maybe`s from the `Applicative` typeclass.

-}
next : Maybe a -> Maybe b -> Maybe b
next =
    map2 (\b a -> always a b)


{-| Take two `Maybe` values. If the second one equals `Nothing`, return `Nothing`. Otherwise return the first value.

    prev (Just 1) (Just 2)
    --> Just 1

    prev Nothing (Just 2)
    --> Nothing

    prev (Just 1) Nothing
    --> Nothing

Advanced functional programmers will recognize this as the implementation of `<*` for `Maybe`s from the `Applicative` typeclass.

-}
prev : Maybe a -> Maybe b -> Maybe a
prev =
    map2 always
