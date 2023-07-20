module Color.HSLuv.Accessors exposing (iso)

{-| Color.HSLuv.Accessors

@docs hsluv

-}

import Base exposing (Iso)
import Color exposing (Color)
import Color.Round as Round
import HSLuv exposing (HSLuv)


{-| iso: This accessor lets you convert between oklch & avh4/elm-color

    import Accessors exposing (to, from)
    import Color


    from iso <| to iso Color.red
    --> Color.red

-}
iso : Iso pr ls Color HSLuv x y
iso =
    Base.iso "color_hsluv" HSLuv.color (HSLuv.toColor >> Round.rgb)
