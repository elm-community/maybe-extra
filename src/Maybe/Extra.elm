module Maybe.Extra
  ( (?)
  , join
  , isNothing
  , isJust
  , keepJusts
  , maybe
  , unsafe
  , map2, map3, map4, map5
  , mapReplaceBy
  , ap, next, prev
  , or
  , listToMaybe, maybeToList, arrayToMaybe, maybeToArray
  , traverse, sequence, traverseArray, sequenceArray
  ) where
{-| Convenience functions for Maybe.

# Common helpers
@docs (?), join, isNothing, isJust, keepJusts, maybe, unsafe

# Map
@docs map2, map3, map4, map5

# Applicative functions
@docs mapReplaceBy, ap, next, prev, or

# List and array functions
@docs listToMaybe, maybeToList, arrayToMaybe, maybeToArray, traverse, sequence, traverseArray, sequenceArray
-}

import Array
import Debug
import Maybe exposing (..)

{-| Flipped, infix version of `withDefault`.

    head [] ? 0 == 0
-}
(?) : Maybe a -> a -> a
mx ? x = withDefault x mx

{-| Flattens nested Maybes

    join (Just (Just 1)) == Just 1
    join (Just Nothing)  == Nothing
    join Nothing         == Nothing
-}
join : Maybe (Maybe a) -> Maybe a
join mx =
  case mx of
    Just x -> x
    Nothing -> Nothing

{-| Conveniently check if a `Maybe` matches `Nothing`.

    isNothing (Just 42) == False
    isNothing (Just []) == False
    isNothing Nothing   == True
-}
isNothing : Maybe a -> Bool
isNothing m =
  case m of
    Nothing -> True
    Just _  -> False

{-| Conveniently check if a `Maybe` matches `Just _`.

    isJust (Just 42) == True
    isJust (Just []) == True
    isJust Nothing   == False
-}
isJust : Maybe a -> Bool
isJust m =
  case m of
    Nothing -> False
    Just _  -> True

{-| Take a list of `Maybe`s and return the list of all values inside `Just`s.

    keepJusts [Nothing, Just 1, Nothing, Just 2, Just 3] == [1,2,3]
-}
keepJusts : List (Maybe a) -> List a
keepJusts =
  let
    step e acc =
      case e of
        Nothing -> acc
        Just x -> x::acc
  in
    List.foldr step []


{-| Take default value, a function, and a `Maybe` value. If the `Maybe` value is `Nothing`, the function returns the default value. Otherwise, it applies the function to the value of `Just` and returns the result.

    maybe [] ((::)0) Nothing == []
    maybe [] ((::)0) (Just [1,2,3]) == [0,1,2,3]
-}
maybe : b -> (a -> b) -> Maybe a -> b
maybe d f = withDefault d << map f


{-| Extracts the value, if it's `Just`, or throw an error, if it's `Nothing`.
-}
unsafe : Maybe a -> a
unsafe v =
  case v of
    Nothing -> Debug.crash "Can't extract value from Nothing!"
    Just x -> x


{-| Combine two `Maybe`s with the given function. If one of the `Maybe`s is `Nothing`, the result is `Nothing`.

    map2 (+) (Just 1) (Just 2) == Just 3
    map2 (,) (Just 0) (Just 'a') == Just (0, 'a')
-}
map2 : (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
map2 f a b = map f a `ap` b

{-|-}
map3 : (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
map3 f a b c = map f a `ap` b `ap` c

{-|-}
map4 : (a -> b -> c -> d -> e) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e
map4 f a b c d = map f a `ap` b `ap` c `ap` d

{-|-}
map5 : (a -> b -> c -> d -> e -> f) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe f
map5 f a b c d e = map f a `ap` b `ap` c `ap` d `ap` e


{-| Replace the value inside `Just` with a new value (possibly of a different type). Otherwise, return `Nothing`.

Advanced functional programmers will recognize this as the implementation of `<$` for `Maybe`s from the `Functor` typeclass, where `<$ == map << always`.
-}
mapReplaceBy : a -> Maybe b -> Maybe a
mapReplaceBy = map << always


{-| Apply the function that is inside `Maybe` to a value that is inside `Maybe`. Return the result inside `Maybe`. If one of the `Maybe` arguments is `Nothing`, return `Nothing`.

    Just ((+)2) `ap` Just 3 == Just 5
    Just Nothing `ap` Just 3 == Nothing
    Just ((+)2) `ap` Nothing == Nothing

Advanced functional programmers will recognize this as the implementation of `<*>` for `Maybe`s from the `Applicative` typeclass.
-}
ap : Maybe (a -> b) -> Maybe a -> Maybe b
ap f x = x `andThen` (\x' -> f `andThen` (\f' -> Just <| f' x'))


{-| Take two `Maybe` values. If the first one equals `Nothing`, return `Nothing`. Otherwise return the second value.

    next (Just 1) (Just 2) == Just 2
    next Nothing (Just 2) == Nothing
    next (Just 1) Nothing == Nothing

Advanced functional programmers will recognize this as the implementation of `*>` for `Maybe`s from the `Applicative` typeclass.
-}
next : Maybe a -> Maybe b -> Maybe b
next = map2 (flip always)


{-| Take two `Maybe` values. If the second one equals `Nothing`, return `Nothing`. Otherwise return the first value.

    prev (Just 1) (Just 2) == Just 1
    prev Nothing (Just 2) == Nothing
    prev (Just 1) Nothing == Nothing

Advanced functional programmers will recognize this as the implementation of `<*` for `Maybe`s from the `Applicative` typeclass.
-}
prev : Maybe a -> Maybe b -> Maybe a
prev = map2 always


{-|
  Like the boolean '||' this will return the first value that is positive ('Just').

    Just 4 `or` Just 5    == Just 4
    Just 4 `or` Nothing   == Just 4
    Nothing `or` Just 5   == Just 5
    Nothing `or` Nothing  == Nothing

  This function sort of works like 'oneOf' but on single 'Maybe's.

  Advanced functional programmers will recognize this as the implementation of 'mplus' for Maybes from the 'MonadPlus' Typeclass.
-}
or : Maybe a -> Maybe a -> Maybe a
or ma mb =
  case ma of
    Nothing -> mb
    Just _ -> ma


{-| Returns `Nothing` on an empty list or `Just a`, where `a` is the first element of the list.

    listToMaybe [] == Nothing
    listToMaybe [1,2,3] == Just 1
-}
listToMaybe : List a -> Maybe a
listToMaybe xs =
  case xs of
    [] -> Nothing
    (x::_) -> Just x


{-| Return an empty list on `Nothing` or a list with one element, where the element is the value of `Just`.

    maybeToList Nothing == []
    maybeToList (Just 1) == [1]
-}
maybeToList : Maybe a -> List a
maybeToList m =
  case m of
    Nothing -> []
    Just x -> [x]


{-| Returns `Nothing` on an empty array or `Just a`, where `a` is the first element of the array.

    arrayToMaybe (Array.fromList []) == Nothing
    arrayToMaybe (Array.fromList [1,2,3]) == Just 1

-}
arrayToMaybe : Array.Array a -> Maybe a
arrayToMaybe = Array.get 0


{-| Return an empty array on `Nothing` or a list with one element, where the element is the value of `Just`.

    maybeToArray Nothing == Array.fromList []
    maybeToArray (Just 1) == Array.fromList [1]

-}
maybeToArray : Maybe a -> Array.Array a
maybeToArray m =
  case m of
    Nothing -> Array.empty
    Just x -> Array.repeat 1 x


{-| Take a function that returns `Maybe` value and a list. Map a function over each element of the list. Collect the result in the list within `Maybe`.

    traverse (\x -> Just (x*10)) [1,2,3,4,5] == Just [10,20,30,40,50]
-}
traverse : (a -> Maybe b) -> List a -> Maybe (List b)
traverse f =
  let
    step e acc =
      case f e of
        Nothing -> Nothing
        Just x -> map ((::)x) acc
  in
    List.foldr step (Just [])


{-| Take a list of `Maybe`s and return a `Maybe` with a list of values. `sequence == traverse identity`.

    sequence [] == Just []
    sequence [Just 1, Just 2, Just 3] == Just [1,2,3]
    sequence [Just 1, Nothing, Just 3] == Nothing
-}
sequence : List (Maybe a) -> Maybe (List a)
sequence = traverse identity


{-|-}
traverseArray : (a -> Maybe b) -> Array.Array a -> Maybe (Array.Array b)
traverseArray f =
  let
    step e acc =
      case f e of
        Nothing -> Nothing
        Just x -> map (Array.push x) acc
  in
    Array.foldl step (Just Array.empty)


{-|-}
sequenceArray : Array.Array (Maybe a) -> Maybe (Array.Array a)
sequenceArray = traverseArray identity
