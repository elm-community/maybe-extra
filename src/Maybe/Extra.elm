module Maybe.Extra where
{-| Convenience functions for Maybe.

@docs join
-}

import Maybe (..)

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
