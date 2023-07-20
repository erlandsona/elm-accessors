module Color.ElmUI.Accessors exposing (color)

import Base exposing (Iso)
import Color exposing (Color)
import Color.Round as Round
import Element as E


{-| color: This accessor lets you convert between oklch & avh4/elm-color

    import Accessors exposing (..)
    import Color.ElmUI.Accessors exposing (..)
    import Color


    from color <| to color Color.red
    --> Color.red

    from color <| to color Color.yellow
    --> Color.yellow

-}
color : Iso pr ls Color E.Color x y
color =
    Base.iso "color_ui" (E.fromRgb << Color.toRgba) (E.toRgb >> Color.fromRgba)
