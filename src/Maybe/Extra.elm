module Maybe.Extra
  ( (?)
  , join
  , isNothing
  , isJust
  , or
  ) where
{-| Convenience functions for Maybe.

@docs (?), join, isNothing, isJust, or
-}

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
