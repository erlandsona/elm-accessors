module VerifyExamples.Accessors.Def0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Accessors exposing (..)
import Lens as L
import Dict exposing (Dict)



dict : Dict String {bar : Int}
dict =
    Dict.fromList [("foo", {bar = 2})]



spec0 : Test.Test
spec0 =
    Test.test "#def: \n\n    get (key \"baz\" << def {bar = 0}) dict\n    --> {bar = 0}" <|
        \() ->
            Expect.equal
                (
                get (key "baz" << def {bar = 0}) dict
                )
                (
                {bar = 0}
                )