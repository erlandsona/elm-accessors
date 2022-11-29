module Base exposing
    ( Optic
    , Iso, Lens, Prism, Traversal
    , iso, lens, prism, traversal
    , get, all, try, has, map, set, new, name, to, from
    , ixd
    , Iso_, Lens_, Prism_, Traversal_
    , An_Optic, An_Iso, A_Lens, A_Prism
    , An_Optic_, An_Iso_, A_Lens_, A_Prism_
    )

{-|


# Optics

@docs Optic
@docs Iso, Lens, Prism, Traversal


# Build your own accessors

Accessors are built using these functions:

@docs iso, lens, prism, traversal


# Actions

@docs get, all, try, has, map, set, new, name, to, from, over


# Accessor Lifters for Indexed operations

@docs ixd


## Type aliases for custom and action functions

@docs Iso_, Lens_, Prism_, Traversal_
@docs An_Optic, An_Iso, A_Lens, A_Prism
@docs An_Optic_, An_Iso_, A_Lens_, A_Prism_

-}


{-| A `Optic value view over` is a type describing how to interact with a
`sub` data when given a `super` data.

The `wrap` exists because some types can't ensure that `get` will return a
`sub`. For instance, `Maybe sub` may not actually contain a `sub`. Therefore,
`get` returns a `wrap` which, in that example, will be `Maybe sub`

Implementation: A relation is a banal record storing a `get` function and an
`over` function.

-}
type Optic_ pr ls s t a b
    = Optic (Internal s t a b)


type alias Internal s t a b =
    { view : s -> a
    , make : b -> t
    , over : (a -> b) -> s -> t
    , list : s -> List a
    , name : String
    }


internal : Optic_ pr ls s t a b -> Internal s t a b
internal (Optic i) =
    i


{-| Any Optic is both "lens" and "prism".
-}
type alias Optic pr ls s t a b x y =
    Optic_ pr ls a b x y -> Optic_ pr ls s t x y


{-| This MUST be a Prism or Iso
-}
type alias An_Optic pr ls s a =
    An_Optic_ pr ls s s a a


{-| Any Optic
-}
type alias An_Optic_ pr ls s t a b =
    Optic pr ls s t a b a b


{-| The isomorphism is both "lens" and "prism".
-}
type alias Iso pr ls s a x y =
    Iso_ pr ls s s a a x y


{-| The isomorphism is both "lens" and "prism".
-}
type alias Iso_ pr ls s t a b x y =
    Optic pr ls s t a b x y


{-| This MUST be a Prism or Iso
-}
type alias An_Iso s a =
    An_Iso_ s s a a


{-| This MUST be a Prism or Iso
-}
type alias An_Iso_ s t a b =
    Optic () () s t a b a b


{-| `Lens` that cannot change type of the object.
-}
type alias Lens ls s a x y =
    Lens_ ls s s a a x y


{-| The lens is "not a prism".
-}
type alias Lens_ ls s t a b x y =
    Optic Never ls s t a b x y


{-| This MUST be a non-type changing Lens or Iso
-}
type alias A_Lens pr s a =
    A_Lens_ pr s s a a


{-| This MUST be a Lens or Iso
-}
type alias A_Lens_ pr s t a b =
    Optic pr () s t a b a b


{-| `Prism` that cannot change the type of the object.
-}
type alias Prism pr s a x y =
    Prism_ pr s s a a x y


{-| The prism is "not a lens".
-}
type alias Prism_ pr s t a b x y =
    Optic pr Never s t a b x y


{-| This MUST be a non-type changing Prism or Iso
-}
type alias A_Prism ls s a =
    A_Prism_ ls s s a a


{-| This MUST be a Prism or Iso
-}
type alias A_Prism_ ls s t a b =
    Optic () ls s t a b a b


{-| `Traversal` that cannot change the type of the object.
-}
type alias Traversal s a x y =
    Traversal_ s s a a x y


{-| The traversal is neither "lens" or "prism".
-}
type alias Traversal_ s t a b x y =
    Optic Never Never s t a b x y


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    foo : Optic attr attrView attrOver -> Optic { rec | foo : attr } view over
    foo =
        Accessors.lens ".foo" .foo (\rec new -> { rec | foo = new })

-}
lens :
    String
    -> (s -> a)
    -> (s -> b -> t)
    -- Any optic composed with a Lens becomes "at least a Lens".
    -> Lens_ ls s t a b x y
lens n sa sbt sub =
    let
        over_ : (a -> b) -> s -> t
        over_ f s =
            s |> sa |> f |> sbt s
    in
    Optic
        { list = sa >> List.singleton
        , view = sa
        , make = void "Can't call `make` with a Lens"
        , over = over_
        , name = n
        }
        |> dot sub


{-| A prism constructor.

Parameters are: reconstructor and a splitter.

Reconstructor takes a final value and constructs a final object.

The splitter turns initial object either to final object directly (if initial object is of wrong variant),
or spits out `a`.

-}
prism :
    String
    -> (b -> t)
    -> (s -> Result t a)
    -> Prism_ pr s t a b x y
prism n bt sta sub =
    let
        over_ : (a -> b) -> s -> t
        over_ f =
            sta >> Result.map (f >> bt) >> mergeResult
    in
    Optic
        { view = void "Can't call `view` with a Prism"
        , list =
            sta
                >> Result.map (\a -> [ a ])
                >> Result.withDefault []
        , make = bt
        , over = over_
        , name = n
        }
        |> dot sub


mergeResult : Result a a -> a
mergeResult r =
    case r of
        Ok a ->
            a

        Err a ->
            a


traversal :
    String
    -> (s -> List a)
    -> ((a -> b) -> s -> t)
    -> Traversal_ s t a b x y
traversal n sa abst sub =
    Optic
        { view = void "Can't call `view` with a Traversal"
        , make = void "Can't call `make` with a Traversal"
        , list = sa
        , over = abst
        , name = n
        }
        |> dot sub


{-| An isomorphism constructor.
-}
iso :
    String
    -> (s -> a)
    -> (b -> t)
    -> Iso_ pr ls s t a b x y
iso n sa bt sub =
    let
        over_ : (a -> b) -> s -> t
        over_ f =
            bt << f << sa
    in
    Optic
        { view = sa
        , list = sa >> List.singleton
        , make = bt
        , over = over_
        , name = n
        }
        |> dot sub


dot : Optic_ pr ls u v a b -> Optic_ pr ls s t u v -> Optic_ pr ls s t a b
dot (Optic attribute) (Optic structure) =
    Optic
        { list = structure.list >> List.concatMap attribute.list
        , view = structure.view >> attribute.view
        , make = structure.make << attribute.make
        , over = structure.over << attribute.over
        , name = structure.name ++ attribute.name
        }


ixd : An_Optic_ pr ls s t a b -> Traversal_ ( ix, s ) t a b x y
ixd p =
    traversal (name p)
        (\( _, b ) -> all p b)
        (\fn -> Tuple.mapSecond (map p fn) >> Tuple.second)



-- {-| Get the inverse of an isomorphism
-- -}


from : An_Iso_ s t a b -> b -> t
from =
    get << swap


swap : An_Iso_ s t a b -> An_Iso_ b a t s
swap accessor =
    let
        i =
            id
                |> accessor
                |> internal
    in
    iso (String.reverse i.name)
        i.make
        i.view


{-| Alias of `get` for isomorphisms
-}
to : An_Iso_ s t a b -> s -> a
to =
    get


id : Optic_ pr ls a b a b
id =
    Optic
        { view = identity
        , make = identity
        , over = identity
        , list = List.singleton
        , name = ""
        }



-- Actions


{-| Used with a Prism, think of `!!` boolean coercion in Javascript except type safe.

    Just 1234
        |> has try
    --> True

    Nothing
        |> has try
    --> False

    ["Stuff", "things"]
        |> has (at 2)
    --> False

    ["Stuff", "things"]
        |> has (at 0)
    --> True

-}
get : A_Lens_ pr s t a b -> s -> a
get accessor =
    (id |> accessor |> internal).view


{-| Used with a Prism, think of `!!` boolean coercion in Javascript except type safe.

    Just 1234
        |> has try
    --> True

    Nothing
        |> has try
    --> False

    ["Stuff", "things"]
        |> has (at 2)
    --> False

    ["Stuff", "things"]
        |> has (at 0)
    --> True

-}
has : An_Optic_ ps ls s t a b -> s -> Bool
has accessor s =
    try accessor s /= Nothing


{-| Used with a Prism, think of `!!` boolean coercion in Javascript except type safe.

    ["Stuff", "things"]
        |> try (at 2)
    --> Nothing

    ["Stuff", "things"]
        |> try (at 0)
    --> Just "Stuff"

-}
try : An_Optic_ pr ls s t a b -> s -> Maybe a
try accessor =
    (id |> accessor |> internal).list >> List.head


{-| Used with a Prism, think of `!!` boolean coercion in Javascript except type safe.

    Just "Stuff"
        |> all just
    --> ["Stuff"]

    Nothing
        |> all just
    --> []

-}
all : An_Optic_ pr ls s t a b -> s -> List a
all accessor =
    (id |> accessor |> internal).list


set : An_Optic_ pr ls s t a b -> b -> (s -> t)
set accessor =
    map accessor << always


map : An_Optic_ pr ls s t a b -> (a -> b) -> s -> t
map accessor =
    (id |> accessor |> internal).over


{-| Use prism to reconstruct.
-}
new : A_Prism_ ls s t a b -> b -> t
new accessor =
    (id |> accessor |> internal).make


{-| -}
name : An_Optic_ pr ls s t a b -> String
name accessor =
    (id |> accessor |> internal).name



-- Helper


void : String -> any -> runTimeError
void s _ =
    let
        unoptimizedRecursion : String -> runTimeError
        unoptimizedRecursion str =
            unoptimizedRecursion str
    in
    unoptimizedRecursion s
