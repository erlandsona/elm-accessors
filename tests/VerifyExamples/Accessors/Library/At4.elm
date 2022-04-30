module VerifyExamples.Accessors.Library.At4 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Accessors.Library exposing (..)
import Lens as L
import Accessors.Library exposing (..)
import Accessors exposing (..)



list : List { bar : String }
list = [{ bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" }]



spec4 : Test.Test
spec4 =
    Test.test "#at: \n\n    get (at 1) list\n    --> Just { bar = \"Things\" }" <|
        \() ->
            Expect.equal
                (
                get (at 1) list
                )
                (
                Just { bar = "Things" }
                )