module Spec exposing (suite)

import Accessors exposing (..)
import Accessors.Library exposing (..)
import Dict exposing (Dict)
import Expect
import Lens as L
import Test exposing (Test, describe, test)


simpleRecord : { foo : number, bar : String, qux : Bool }
simpleRecord =
    { foo = 3, bar = "Yop", qux = False }


anotherRecord : { foo : number, bar : String, qux : Bool }
anotherRecord =
    { foo = 5, bar = "Sup", qux = True }


nestedRecord : { foo : { foo : number, bar : String, qux : Bool } }
nestedRecord =
    { foo = simpleRecord }


recordWithList : { bar : List { foo : number, bar : String, qux : Bool } }
recordWithList =
    { bar = [ simpleRecord, anotherRecord ] }


maybeRecord : { bar : Maybe { foo : number, bar : String, qux : Bool }, foo : Maybe a }
maybeRecord =
    { bar = Just simpleRecord, foo = Nothing }


dict : Dict String number
dict =
    Dict.fromList [ ( "foo", 7 ) ]


recordWithDict : { bar : Dict String number }
recordWithDict =
    { bar = dict }


dictWithRecord : Dict String { bar : String }
dictWithRecord =
    Dict.fromList [ ( "foo", { bar = "Yop" } ) ]


suite : Test
suite =
    describe "strict lenses"
        [ describe "get"
            [ test "simple get" <|
                \_ ->
                    get L.foo simpleRecord
                        |> Expect.equal 3
            , test "nested get" <|
                \_ ->
                    get (L.foo << L.bar) nestedRecord
                        |> Expect.equal "Yop"
            , test "get in list" <|
                \_ ->
                    get (L.bar << onEach << L.foo) recordWithList
                        |> Expect.equal [ 3, 5 ]
            , test "get in Just" <|
                \_ ->
                    get (L.bar << try << L.qux) maybeRecord
                        |> Expect.equal (Just False)
            , test "get in Nothing" <|
                \_ ->
                    get (L.foo << try << L.bar) maybeRecord
                        |> Expect.equal Nothing
            , describe "dict"
                [ test "get present" <|
                    \_ ->
                        get (key "foo") dict
                            |> Expect.equal (Just 7)
                , test "get absent" <|
                    \_ ->
                        get (key "bar") dict
                            |> Expect.equal Nothing
                , test "nested get present" <|
                    \_ ->
                        get (L.bar << key "foo") recordWithDict
                            |> Expect.equal (Just 7)
                , test "nested get absent" <|
                    \_ ->
                        get (L.bar << key "bar") recordWithDict
                            |> Expect.equal Nothing
                , test "get with try" <|
                    \_ ->
                        get (key "foo" << try << L.bar) dictWithRecord
                            |> Expect.equal (Just "Yop")
                , test "get with def" <|
                    \_ ->
                        dictWithRecord
                            |> get (key "not_it" << def { bar = "Stuff" } << L.bar)
                            |> Expect.equal "Stuff"
                , test "get with or" <|
                    \_ ->
                        dictWithRecord
                            |> get ((key "not_it" << try << L.bar) |> or "Stuff")
                            |> Expect.equal "Stuff"
                ]
            ]
        , describe "set"
            [ test "simple set" <|
                \_ ->
                    let
                        updatedExample : { foo : number, bar : String, qux : Bool }
                        updatedExample =
                            set L.qux True simpleRecord
                    in
                    updatedExample.qux
                        |> Expect.equal True
            , test "nested set" <|
                \_ ->
                    let
                        updatedExample : { foo : { foo : number, bar : String, qux : Bool } }
                        updatedExample =
                            set (L.foo << L.foo) 5 nestedRecord
                    in
                    updatedExample.foo.foo
                        |> Expect.equal 5
            , test "set in list" <|
                \_ ->
                    let
                        updatedExample : { bar : List { foo : number, bar : String, qux : Bool } }
                        updatedExample =
                            set (L.bar << onEach << L.bar) "Why, hello" recordWithList
                    in
                    get (L.bar << onEach << L.bar) updatedExample
                        |> Expect.equal [ "Why, hello", "Why, hello" ]
            , test "set in Just" <|
                \_ ->
                    let
                        updatedExample : { bar : Maybe { foo : number, bar : String, qux : Bool }, foo : Maybe a }
                        updatedExample =
                            set (L.bar << try << L.foo) 4 maybeRecord
                    in
                    get (L.bar << try << L.foo) updatedExample
                        |> Expect.equal (Just 4)
            , test "set in Nothing" <|
                \_ ->
                    let
                        -- updatedExample : { bar : Maybe { foo : number, bar : String, qux : Bool }, foo : Maybe a }
                        updatedExample =
                            set (L.foo << try << L.bar) "Nope" maybeRecord
                    in
                    get (L.foo << try << L.bar) updatedExample
                        |> Expect.equal Nothing
            , describe "dict"
                [ test "set currently present to present" <|
                    \_ ->
                        let
                            updatedDict : Dict String number
                            updatedDict =
                                set (key "foo") (Just 9) dict
                        in
                        get (key "foo") updatedDict |> Expect.equal (Just 9)
                , test "set currently absent to present" <|
                    \_ ->
                        let
                            updatedDict : Dict String number
                            updatedDict =
                                set (key "bar") (Just 9) dict
                        in
                        get (key "bar") updatedDict |> Expect.equal (Just 9)
                , test "set currently present to absent" <|
                    \_ ->
                        let
                            updatedDict : Dict String number
                            updatedDict =
                                set (key "foo") Nothing dict
                        in
                        get (key "foo") updatedDict |> Expect.equal Nothing
                , test "set currently absent to absent" <|
                    \_ ->
                        let
                            updatedDict : Dict String number
                            updatedDict =
                                set (key "bar") Nothing dict
                        in
                        get (key "bar") updatedDict |> Expect.equal Nothing
                , test "set with try present" <|
                    \_ ->
                        let
                            updatedDict : Dict String { bar : String }
                            updatedDict =
                                set (key "foo" << try << L.bar) "Sup" dictWithRecord
                        in
                        get (key "foo" << try << L.bar) updatedDict |> Expect.equal (Just "Sup")
                , test "set with try absent" <|
                    \_ ->
                        let
                            updatedDict : Dict String { bar : String }
                            updatedDict =
                                set (key "bar" << try << L.bar) "Sup" dictWithRecord
                        in
                        get (key "bar" << try << L.bar) updatedDict |> Expect.equal Nothing
                ]
            ]
        , describe "over"
            [ test "simple over" <|
                \_ ->
                    let
                        updatedExample : { foo : number, bar : String, qux : Bool }
                        updatedExample =
                            over L.bar (\w -> w ++ " lait") simpleRecord
                    in
                    updatedExample.bar
                        |> Expect.equal "Yop lait"
            , test "nested over" <|
                \_ ->
                    let
                        updatedExample : { foo : { foo : number, bar : String, qux : Bool } }
                        updatedExample =
                            over (L.foo << L.qux) (\w -> not w) nestedRecord
                    in
                    updatedExample.foo.qux
                        |> Expect.equal True
            , test "over list" <|
                \_ ->
                    let
                        updatedExample : { bar : List { foo : number, bar : String, qux : Bool } }
                        updatedExample =
                            over (L.bar << onEach << L.foo) (\n -> n - 2) recordWithList
                    in
                    get (L.bar << onEach << L.foo) updatedExample
                        |> Expect.equal [ 1, 3 ]
            , test "over through Just" <|
                \_ ->
                    let
                        updatedExample : { bar : Maybe { foo : number, bar : String, qux : Bool }, foo : Maybe a }
                        updatedExample =
                            over (L.bar << try << L.foo) (\n -> n + 3) maybeRecord
                    in
                    get (L.bar << try << L.foo) updatedExample
                        |> Expect.equal (Just 6)
            , test "over through Nothing" <|
                \_ ->
                    let
                        -- updatedExample : { bar : Maybe { foo : number, bar : String, qux : Bool }, foo : Maybe a }
                        updatedExample =
                            over (L.foo << try << L.bar) (\w -> w ++ "!") maybeRecord
                    in
                    get (L.foo << try << L.bar) updatedExample
                        |> Expect.equal Nothing
            ]
        , describe "making accessors"
            [ let
                myFoo =
                    makeOneToOne .foo (\f rec -> { rec | foo = f rec.foo })
              in
              describe "makeOneToOne"
                [ test "get" <|
                    \_ ->
                        get (myFoo << L.bar) nestedRecord
                            |> Expect.equal "Yop"
                , test "set" <|
                    \_ ->
                        let
                            updatedRec : { foo : { foo : number, bar : String, qux : Bool } }
                            updatedRec =
                                set (L.foo << myFoo) 1 nestedRecord
                        in
                        updatedRec.foo.foo |> Expect.equal 1
                , test "over" <|
                    \_ ->
                        let
                            updatedRec : { foo : { foo : number, bar : String, qux : Bool } }
                            updatedRec =
                                over (myFoo << myFoo) (\n -> n + 3) nestedRecord
                        in
                        updatedRec.foo.foo |> Expect.equal 6
                ]
            , let
                myOnEach =
                    makeOneToN List.map List.map
              in
              describe "makeOneToN"
                [ test "get" <|
                    \_ ->
                        get (L.bar << myOnEach << L.foo) recordWithList
                            |> Expect.equal [ 3, 5 ]
                , test "set" <|
                    \_ ->
                        let
                            updatedExample : { bar : List { foo : number, bar : String, qux : Bool } }
                            updatedExample =
                                set (L.bar << myOnEach << L.bar) "Greetings" recordWithList
                        in
                        get (L.bar << onEach << L.bar) updatedExample
                            |> Expect.equal [ "Greetings", "Greetings" ]
                , test "over" <|
                    \_ ->
                        let
                            updatedExample : { bar : List { foo : number, bar : String, qux : Bool } }
                            updatedExample =
                                over (L.bar << myOnEach << L.foo) (\n -> n - 2) recordWithList
                        in
                        get (L.bar << onEach << L.foo) updatedExample
                            |> Expect.equal [ 1, 3 ]
                ]
            ]
        ]
