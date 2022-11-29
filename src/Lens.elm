module Lens exposing (..)

import Base exposing (Lens, lens)


bar : Lens ls { m | bar : a } a x y
bar =
    lens ".bar" .bar (\rec val -> { rec | bar = val })


foo : Lens ls { m | foo : a } a x y
foo =
    lens ".foo" .foo (\rec val -> { rec | foo = val })


qux : Lens ls { m | qux : a } a x y
qux =
    lens ".qux" .qux (\rec val -> { rec | qux = val })


name : Lens ls { m | name : a } a x y
name =
    lens ".name" .name (\rec val -> { rec | name = val })


age : Lens ls { m | age : a } a x y
age =
    lens ".age" .age (\rec val -> { rec | age = val })


email : Lens ls { m | email : a } a x y
email =
    lens ".email" .email (\rec val -> { rec | email = val })


stuff : Lens ls { m | stuff : a } a x y
stuff =
    lens ".stuff" .stuff (\rec val -> { rec | stuff = val })


things : Lens ls { m | things : a } a x y
things =
    lens ".things" .things (\rec val -> { rec | things = val })


info : Lens ls { m | info : a } a x y
info =
    lens ".info" .info (\rec val -> { rec | info = val })
