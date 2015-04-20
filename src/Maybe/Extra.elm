module Maybe.Extra ((?), join, isNothing, isJust) where
{-| Convenience functions for Maybe.

@docs (?), join, isNothing, isJust
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
