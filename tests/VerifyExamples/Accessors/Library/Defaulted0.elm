module VerifyExamples.Accessors.Library.Defaulted0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Accessors.Library exposing (..)
import Lens as L
import Dict exposing (Dict)
import Accessors.Library exposing (..)
import Accessors exposing (..)



dict : Dict String {bar : Int}
dict =
    Dict.fromList [("foo", {bar = 2})]



spec0 : Test.Test
spec0 =
    Test.test "#defaulted: \n\n    over (key \"baz\" << defaulted { bar = 0 } << L.bar) ((+) 1) dict\n    --> Dict.fromList [(\"foo\", { bar = 2 }), (\"baz\", { bar = 1 })]" <|
        \() ->
            Expect.equal
                (
                over (key "baz" << defaulted { bar = 0 } << L.bar) ((+) 1) dict
                )
                (
                Dict.fromList [("foo", { bar = 2 }), ("baz", { bar = 1 })]
                )