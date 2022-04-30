module VerifyExamples.Accessors.Library.Key1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Accessors.Library exposing (..)
import Lens as L
import Accessors.Library exposing (..)
import Accessors exposing (..)
import Dict exposing (Dict)



dict : Dict String {bar : Int}
dict = Dict.fromList [("foo", {bar = 2})]



spec1 : Test.Test
spec1 =
    Test.test "#key: \n\n    set (key \"foo\") Nothing dict\n    --> Dict.remove \"foo\" dict" <|
        \() ->
            Expect.equal
                (
                set (key "foo") Nothing dict
                )
                (
                Dict.remove "foo" dict
                )