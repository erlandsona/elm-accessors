module VerifyExamples.Accessors.Library.OnEveryIx0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Accessors.Library exposing (..)
import Array exposing (Array)
import Lens as L
import Accessors.Library exposing (..)
import Accessors exposing (..)



multiplyIfGTOne : (Int, { bar : Int }) -> (Int, { bar : Int })
multiplyIfGTOne ( idx, ({ bar } as rec) ) =
    if idx > 0 then
        ( idx, { bar = bar * 10 } )
    else
        (idx, rec)
arrayRecord : { foo : Array { bar : Int } }
arrayRecord = { foo = [ {bar = 2}
                      , {bar = 3}
                      , {bar = 4}
                      ] |> Array.fromList
              }



spec0 : Test.Test
spec0 =
    Test.test "#onEveryIx: \n\n    over (L.foo << onEveryIx << snd << L.bar) ((+) 1) arrayRecord\n    --> {foo = [{bar = 3}, {bar = 4}, {bar = 5}] |> Array.fromList}" <|
        \() ->
            Expect.equal
                (
                over (L.foo << onEveryIx << snd << L.bar) ((+) 1) arrayRecord
                )
                (
                {foo = [{bar = 3}, {bar = 4}, {bar = 5}] |> Array.fromList}
                )