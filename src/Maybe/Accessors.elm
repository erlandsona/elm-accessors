module Maybe.Accessors exposing (just)

{-|

@docs just

-}

import Base exposing (Prism_)


{-| This accessor combinator lets you access values inside Maybe.
see [`try_`](Maybe-Accessors#try_) for a flattening lens.

    import Base exposing (try, map)
    import Dict exposing (Dict)
    import Dict.Accessors as Dict
    import Maybe.Accessors as Maybe
    import Lens as L

    maybeRecord : { foo : Maybe { bar : Maybe {stuff : Maybe Int} }, qux : Maybe { bar : Maybe Int } }
    maybeRecord = { foo = Just { bar = Just { stuff = Just 2 } }
                  , qux = Nothing
                  }

    try (L.foo << Maybe.just << L.bar << Maybe.just << L.stuff) maybeRecord
    --> Just (Just 2)

    try (L.qux << Maybe.just << L.bar) maybeRecord
    --> Nothing

    map (L.foo << Maybe.just << L.bar << Maybe.just << L.stuff << Maybe.just) ((+) 1) maybeRecord
    --> {foo = Just {bar = Just { stuff = Just 3 }}, qux = Nothing}

    map (L.qux << Maybe.just << L.bar << Maybe.just) ((+) 1) maybeRecord
    --> {foo = Just {bar = Just {stuff = Just 2}}, qux = Nothing}

-}
just : Prism_ pr (Maybe a) (Maybe b)  a b x y
just =
    Base.prism "?" Just (Result.fromMaybe Nothing)
