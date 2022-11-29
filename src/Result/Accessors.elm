module Result.Accessors exposing (ok, err)

{-|

@docs ok, err

-}

import Base exposing (Prism_)
import Result exposing (Result(..))


{-| This accessor lets you access values inside the Ok variant of a Result.

    import Accessors exposing (..)
    import Result.Accessors as Result
    import Lens as L

    maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
    maybeRecord = { foo = Ok { bar = 2 }
                  , qux = Err "Not an Int"
                  }

    try (L.foo << Result.ok << L.bar) maybeRecord
    --> Just 2

    try (L.qux << Result.ok << L.bar) maybeRecord
    --> Nothing

    map (L.foo << Result.ok << L.bar) ((+) 1) maybeRecord
    --> { foo = Ok { bar = 3 }, qux = Err "Not an Int" }

    map (L.qux << Result.ok << L.bar) ((+) 1) maybeRecord
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

-}
ok : Prism_ pr (Result ignored a) (Result ignored b)  a b x y
ok =
    Base.prism ".?[Ok]" Ok (unwrap (Err >> Err) Ok)


{-| This accessor lets you access values inside the Err variant of a Result.

    import Accessors exposing (..)
    import Result.Accessors as Result
    import Lens as L

    maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
    maybeRecord = { foo = Ok { bar = 2 }
                  , qux = Err "Not an Int"
                  }

    try (L.foo << Result.err) maybeRecord
    --> Nothing

    try (L.qux << Result.err) maybeRecord
    --> Just "Not an Int"

    map (L.foo << Result.err) String.toUpper maybeRecord
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

    map (L.qux << Result.err) String.toUpper maybeRecord
    --> { foo = Ok { bar = 2 }, qux = Err "NOT AN INT" }

-}
err : Prism_ pr (Result a ignored) (Result b ignored)  a b x y
err =
    Base.prism ".?[Err]" Err (unwrap Ok (Ok >> Err))


unwrap : (e -> x) -> (a -> x) -> Result e a -> x
unwrap onE onA r =
    case r of
        Ok a ->
            onA a

        Err e ->
            onE e
