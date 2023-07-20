module Color.HSLuv.Accessors exposing (color)

import Base exposing (Iso)
import Color exposing (Color)
import Color.Round as Round
import HSLuv exposing (HSLuv)


{-| color: This accessor lets you convert between oklch & avh4/elm-color

    import Accessors exposing (..)
    import Color.HSLuv.Accessors exposing (..)
    import Color


    from color <| to color Color.red
    --> Color.red

-}
color : Iso pr ls Color HSLuv x y
color =
    Base.iso "color_hsluv" HSLuv.color (HSLuv.toColor >> Round.rgb)
