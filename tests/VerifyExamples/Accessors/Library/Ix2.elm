module VerifyExamples.Accessors.Library.Ix2 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Accessors.Library exposing (..)
import Lens as L
import Accessors.Library exposing (..)
import Accessors exposing (..)
import Array exposing (Array)



arr : Array { bar : String }
arr = Array.fromList [{ bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" }]



spec2 : Test.Test
spec2 =
    Test.test "#ix: \n\n    get (ix 0 << L.bar) arr\n    --> Just \"Stuff\"" <|
        \() ->
            Expect.equal
                (
                get (ix 0 << L.bar) arr
                )
                (
                Just "Stuff"
                )