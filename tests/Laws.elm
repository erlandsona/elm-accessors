module Laws exposing (..)

import Accessors as A
    exposing
        ( A_Lens
        , A_Prism
        , An_Iso
        , An_Optic
        , An_Optic_
        , Iso
        , Iso_
        , from
        , iso
        , new
        , to
        , try
        )
import Array exposing (Array)
import Base exposing (A_Lens, Optic)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, string)
import Lens as L
import Maybe exposing (Maybe)
import String
import Test exposing (..)


eq : a -> a -> Expectation
eq =
    Expect.equal


type alias Person =
    { name : String
    , age : Int
    , email : Maybe String
    , stuff : List String
    , info : Dict String String
    , things : Array String
    }


suite : Test
suite =
    describe "Suite!"
        [ test "Name compositions output `jq` style String's" <|
            \() ->
                A.name (L.info << L.stuff << A.at 7 << L.name)
                    |> eq ".info.stuff[7]?.name"
        , describe "Laws Specs"
            [ isSetter (L.email << A.just) personFuzzer strFun string
            , isSetter (L.stuff << A.at 0) personFuzzer strFun string
            , isSetter (L.stuff << A.each) personFuzzer strFun string
            , isSetter (L.things << A.ix 0) personFuzzer strFun string
            , isSetter (L.things << A.every) personFuzzer strFun string
            , isLens L.name personFuzzer strFun string
            , isLens L.age personFuzzer intFun int
            , isLens (L.info << A.key "stuff") personFuzzer maybeStrFun (Fuzz.maybe string)
            , isPrism A.just (Fuzz.maybe string) string
            , isPrism A.ok (Fuzz.result int string) string
            , isPrism A.err (Fuzz.result int string) int
            , isIso intMaybe { s = Fuzz.maybe Fuzz.unit, a = Fuzz.bool, endo = boolFun }
            ]
        ]


intMaybe : Iso pr ls (Maybe ()) Bool x y
intMaybe =
    iso "nulllyInt"
        (\maybeh ->
            case maybeh of
                Just () ->
                    True

                Nothing ->
                    False
        )
        (\true ->
            if true then
                Just ()

            else
                Nothing
        )


type alias Function a =
    a -> a


strFun : Fuzzer (Function String)
strFun =
    Fuzz.oneOf
        -- [ Fuzz.map String.reverse string
        -- , String.toUpper
        -- , String.toLower
        [ Fuzz.map String.append string
        , Fuzz.map (\s -> String.append s >> String.reverse) string
        , Fuzz.map (\s -> String.append s >> String.toUpper) string
        , Fuzz.map (\s -> String.append s >> String.toLower) string
        ]


intFun : Fuzzer (Function Int)
intFun =
    Fuzz.oneOf
        [ Fuzz.map (+) int
        , Fuzz.map (-) int
        , Fuzz.map (*) int
        , Fuzz.map (//) int
        ]


boolFun : Fuzzer (Function Bool)
boolFun =
    Fuzz.oneOf
        [ Fuzz.map (&&) Fuzz.bool
        , Fuzz.map (||) Fuzz.bool
        ]


maybeStrFun : Fuzzer (Function (Maybe String))
maybeStrFun =
    Fuzz.oneOf
        [ Fuzz.map
            (\_ ->
                Maybe.andThen String.toInt
                    >> Maybe.map String.fromInt
            )
            (Fuzz.maybe string)
        ]


personFuzzer : Fuzzer Person
personFuzzer =
    Fuzz.map (\_ -> Person) Fuzz.unit
        |> Fuzz.andMap string
        |> Fuzz.andMap int
        |> Fuzz.andMap (Fuzz.maybe string)
        |> Fuzz.andMap (Fuzz.list string)
        |> Fuzz.andMap (Fuzz.list (Fuzz.tuple ( string, string )) |> Fuzz.map Dict.fromList)
        |> Fuzz.andMap (Fuzz.list string |> Fuzz.map Array.fromList)


isSetter : Optic pr ls a a c c c c -> Fuzzer a -> Fuzzer (c -> c) -> Fuzzer c -> Test
isSetter l fzr fnFzr val =
    describe ("isSetable: " ++ A.name l)
        [ fuzz fzr
            "identity"
            (Expect.true "setter"
                << setter_id l
            )
        , fuzz (Fuzz.tuple3 ( fzr, fnFzr, fnFzr ))
            "composition"
            (\( s, f, g ) ->
                Expect.true "setter" <|
                    setter_composition l s f g
            )
        , fuzz (Fuzz.tuple3 ( fzr, val, val ))
            "set_set"
            (\( s, a, b ) ->
                Expect.true "setter" <|
                    setter_set_set l s a b
            )
        ]


isLens : A_Lens pr a b -> Fuzzer a -> Fuzzer (b -> b) -> Fuzzer b -> Test
isLens l fzr valFn val =
    describe ("isLens: " ++ A.name l)
        [ isSetter l fzr valFn val

        -- There's Traversal laws in here somewhere but not sure they're expressible in Elm.
        , fuzz fzr "lens_set_get" (lens_set_get l >> Expect.true "lens_set_get")
        , fuzz (Fuzz.tuple ( fzr, val ))
            "lens_get_set"
            (\( b, s ) ->
                lens_get_set l b s
                    |> Expect.true "lens_get_set"
            )
        ]


isPrism : A_Prism ls s a -> Fuzzer s -> Fuzzer a -> Test
isPrism pr fzrS fzrA =
    describe ("isPrism: " ++ A.name pr)
        [ fuzz (Fuzz.tuple ( fzrS, fzrA ))
            "yin"
            (\( s, a ) ->
                Expect.true "yin & yang"
                    (prism_yin pr a && prism_yang pr s)
            )

        -- , isTraversal
        ]


isIso :
    An_Iso s a
    ->
        { a : Fuzzer a
        , endo : Fuzzer (a -> a)
        , s : Fuzzer s
        }
    -> Test
isIso i fzrs =
    describe ("isIso: " ++ A.name i)
        [ isPrism i fzrs.s fzrs.a
        , isLens i fzrs.s fzrs.endo fzrs.a
        , fuzz fzrs.s "iso_hither" (Expect.true "hither" << iso_hither i)
        , fuzz fzrs.a "iso_yon" (Expect.true "yon" << iso_yon i)
        ]


setter_id : An_Optic pr ls s a -> s -> Bool
setter_id l s =
    A.map l identity s == s


setter_composition : An_Optic pr ls s a -> s -> (a -> a) -> (a -> a) -> Bool
setter_composition l s f g =
    A.map l f (A.map l g s) == A.map l (f << g) s


setter_set_set : An_Optic pr ls s a -> s -> a -> a -> Bool
setter_set_set l s a b =
    A.set l b (A.set l a s) == A.set l b s


lens_set_get : A_Lens pr b a -> b -> Bool
lens_set_get l s =
    A.set l (A.get l s) s == s


lens_get_set : A_Lens pr s a -> s -> a -> Bool
lens_get_set l s a =
    A.get l (A.set l a s) == a


prism_yin : A_Prism ls s a -> a -> Bool
prism_yin l a =
    try l (new l a) == Just a


prism_yang : A_Prism ls s a -> s -> Bool
prism_yang l s =
    (Maybe.withDefault s <| Maybe.map (new l) (try l s)) == s


iso_hither : An_Iso s a -> s -> Bool
iso_hither l s =
    (from l <| to l s) == s


iso_yon : An_Iso s a -> a -> Bool
iso_yon l a =
    (to l <| from l a) == a



-- traverse_pure : LensLike' f s a -> s -> Bool
-- traverse_pure l s = l pure s == (pure s : f s)
-- traverse_pureMaybe : Eq s => LensLike' Maybe s a -> s -> Bool
-- traverse_pureMaybe = traverse_pure
-- traverse_pureList : Eq s => LensLike' [] s a -> s -> Bool
-- traverse_pureList = traverse_pure
-- traverse_compose : (Applicative f, Applicative g, Eq (f (g s)))
--                     => Traversal' s a -> (a -> g a) -> (a -> f a) -> s -> Bool
-- traverse_compose t f g s = (fmap (t f) . t g) s == (getCompose . t (Compose . fmap f . g)) s
