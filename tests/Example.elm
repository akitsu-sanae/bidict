module Example exposing (suite)

import Bidict
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "empty bidict"
        [ test "created empty is empty" <|
            \_ ->
                Expect.equal Bidict.empty
                    { key2Value = Dict.empty
                    , value2Key = Dict.empty
                    }
        , test "singleton is same inserting to empty" <|
            \_ ->
                Expect.equal
                    (Bidict.singleton "truth" 42)
                    (Bidict.insert "truth" 42 Bidict.empty)
        , test "size of empty is 0" <|
            \_ ->
                Expect.equal
                    (Bidict.size Bidict.empty)
                    0
        ]
