module VerifyExamples.Accessors.Library.Defaulted2 exposing (..)

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



spec2 : Test.Test
spec2 =
    Test.test "#defaulted: \n\n    get (key \"baz\" << defaulted {bar = 0}) dict\n    --> {bar = 0}" <|
        \() ->
            Expect.equal
                (
                get (key "baz" << defaulted {bar = 0}) dict
                )
                (
                {bar = 0}
                )