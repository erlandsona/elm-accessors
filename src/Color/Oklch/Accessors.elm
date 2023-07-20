module Color.Oklch.Accessors exposing (color, oklab)

{-| Color.Oklch.Accessors

@docs color, oklab

-}

import Base exposing (Iso)
import Color exposing (Color)
import Color.Oklab exposing (Oklab)
import Color.Oklch
    exposing
        ( Oklch
        , fromColor
        , fromOklab
        , toColor
        , toOklab
        )
import Color.Round as Round


{-| color: This accessor lets you convert between oklch & avh4/elm-color

    import Accessors exposing (..)
    import Color.Oklch.Accessors exposing (..)
    import Color


    from color <| to color Color.red
    --> Color.red

-}
color : Iso pr ls Color Oklch x y
color =
    Base.iso "color_oklch" fromColor (toColor >> Round.rgb)


{-| oklab: This accessor lets you convert between oklch & avh4/elm-color

    import Accessors exposing (to, from)
    import Color


    from (color << oklab) <| to (color << oklab) Color.red
    --> Color.red

-}
oklab : Iso pr ls Oklch Oklab x y
oklab =
    Base.iso "oklch_oklab" toOklab fromOklab
