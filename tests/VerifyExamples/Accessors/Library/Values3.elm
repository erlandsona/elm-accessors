module VerifyExamples.Accessors.Library.Values3 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Accessors.Library exposing (..)
import Dict exposing (Dict)
import Lens as L
import Accessors.Library exposing (..)
import Accessors exposing (..)



dictRecord : {foo : Dict String {bar : Int}}
dictRecord = { foo = [ ("a", { bar = 2 })
                     , ("b", { bar = 3 })
                     , ("c", { bar = 4 })
                     ] |> Dict.fromList
             }



spec3 : Test.Test
spec3 =
    Test.test "#values: \n\n    get (L.foo << values) dictRecord\n    --> [(\"a\", {bar = 2}), (\"b\", {bar = 3}), (\"c\", {bar = 4})] |> Dict.fromList" <|
        \() ->
            Expect.equal
                (
                get (L.foo << values) dictRecord
                )
                (
                [("a", {bar = 2}), ("b", {bar = 3}), ("c", {bar = 4})] |> Dict.fromList
                )