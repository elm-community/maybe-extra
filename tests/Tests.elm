module Tests exposing (suite)

import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Maybe.Extra"
        [ test "placeholder" <|
            \() ->
                Expect.pass
        ]
