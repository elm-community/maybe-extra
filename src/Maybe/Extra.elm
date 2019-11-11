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


{-|

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


{-|

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


{-| Keep the `Maybe` only if the predicate function passes

    filter (\v -> v == 1) (Just 1)
    --> Just 1

    filter (\v -> v == 2) (Just 1)
    --> Nothing

    filter (\v -> v == 1) Nothing
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


{-| Apply the function to the value in the `Maybe` and return it unwrapped.
If the `Maybe` is `Nothing`, use the default value instead.

`unwrap default f` is equivalent to `Maybe.map f >> Maybe.withDefault default`.

    unwrap 0 String.length Nothing
    --> 0

    unwrap 0 String.length (Just "abc")
    --> 3

-}
unwrap : b -> (a -> b) -> Maybe a -> b
unwrap default f m =
    case m of
        Nothing ->
            default

        Just a ->
            f a


{-| `unpack`, but the default value is lazy,
and will only be computed if the `Maybe` is `Nothing`.

    unpack (\() -> 0) String.length Nothing
    --> 0

    unpack (\() -> 0) String.length (Just "abc")
    --> 3

-}
unpack : (() -> b) -> (a -> b) -> Maybe a -> b
unpack default f m =
    case m of
        Nothing ->
            default ()

        Just a ->
            f a



-- Or: Combine 2 `Maybe`s


{-| Returns the first value that is present, like the boolean `||`.

Both values will be computed. There is no short-circuiting.
If your second argument is expensive to calculate and you need short circuiting, use `orLazy` instead.

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
Piping-friendly version of `orLazy`.

The first argument will only be evaluated if the second argument is `Nothing`.

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


{-| Take all the values that are present, throwing away any `Nothing`s.

Equivalent to `List.filterMap identity`.

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


{-| If every `Maybe` in the list is present, return all of the values unwrapped.
If there are any `Nothing`s, the whole function fails and returns `Nothing`.

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


{-| Like `combine`, but map a function over each element of the list first.

If every function call succeeds (returns `Just`), `traverse` will return a list.
If any function call fails (returns `Nothing`), `traverse` will return `Nothing`.

`combine` is equivalent to `traverse identity`.

    traverse (\x -> Just (x * 10)) [ 1, 2, 3, 4, 5 ]
    --> Just [ 10, 20, 30, 40, 50 ]

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


{-| A `Maybe` is a lot like a list that can only be length 0 or 1.

Returns a singleton list if the value is present, and an empty list it's missing.

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


{-| Like `toList`, but returns a singleton or empty `Array`.

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


{-| If both a function and a value are present, apply the function to the value.
If either argument is `Nothing`, return `Nothing`.

    Just ((+) 2)
        |> andMap (Just 3)
    --> Just 5

    Nothing
        |> andMap (Just 3)
    --> Nothing

    Just ((+) 2)
        |> andMap Nothing
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
