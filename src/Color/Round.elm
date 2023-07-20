module Color.Round exposing (..)

import Color exposing (Color)
import Round exposing (roundNum)


type alias RGBA a =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : a
    }


mapAlpha : (a -> b) -> RGBA a -> RGBA b
mapAlpha fn r =
    { alpha = fn r.alpha
    , red = r.red
    , green = r.green
    , blue = r.blue
    }


rgba255ToDecimal :
    RGBA Float -- RGB255 values between 0 & 255
    -> RGBA Float -- Values clamped between 0 .. 1
rgba255ToDecimal x =
    { x
        | red = x.red / 255
        , green = x.green / 255
        , blue = x.blue / 255
    }


decimalTo255 :
    RGBA Float -- Values clamped between 0 .. 1
    -> RGBA Float -- RGB255 values between 0 & 255
decimalTo255 x =
    { x
        | red = roundNum 0 (x.red * 255)
        , green = roundNum 0 (x.green * 255)
        , blue = roundNum 0 (x.blue * 255)
    }


rgb : Color -> Color
rgb clr =
    let
        r =
            clr |> Color.toRgba
    in
    { alpha = r.alpha
    , red = roundNum 2 r.red
    , green = roundNum 2 r.green
    , blue = roundNum 2 r.blue
    }
        |> Color.fromRgba
