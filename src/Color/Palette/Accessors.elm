module Color.Palette.Accessors exposing (hexA, solid, transparent)

{-| Color.Palette.Accessors

@docs hexA, solid, transparent

-}

import Base exposing (Iso, Prism)
import Color exposing (Color)
import Color.Round as Round
import SolidColor exposing (SolidColor)
import TransparentColor exposing (TransparentColor)


{-| transparent: This accessor lets you convert between avh4/elm-color & tesk9/palette TransparentColor

    import Accessors exposing (to, from)
    import Color


    from transparent <| to transparent Color.red
    --> Color.red

-}
transparent : Iso pr ls Color TransparentColor x y
transparent =
    Base.iso "color_transparent"
        paletteFromColor
        (TransparentColor.toRGBA
            >> Round.mapAlpha TransparentColor.opacityToFloat
            >> Round.rgba255ToDecimal
            >> Color.fromRgba
        )


{-| solid: This accessor lets you convert between tesk9/palette TransparentColor && SolidColor

    import Accessors exposing (to, from)
    import Color


    from (transparent << solid) <| to (transparent << solid) Color.red
    --> Color.red

-}
solid : Iso pr ls TransparentColor SolidColor x y
solid =
    Base.iso "color_solid" TransparentColor.toColor (TransparentColor.fromColor TransparentColor.opaque)


{-| hex: This accessor lets you convert between tesk9/palette TransparentColor && SolidColor

    import Accessors exposing (swap, new, try)
    import Color


    new (hexA << swap transparent) Color.red
    -->  "#CC0000"
    try (hexA << swap transparent) "#C00F" -- with alpha channel
    --> Just Color.red

-}
hexA : Prism pr String TransparentColor x y
hexA =
    Base.prism "color_hexa"
        TransparentColor.toHexA
        TransparentColor.fromHexA


paletteFromColor : Color -> TransparentColor
paletteFromColor =
    Color.toRgba
        >> Round.decimalTo255
        >> Round.mapAlpha TransparentColor.customOpacity
        >> TransparentColor.fromRGBA
