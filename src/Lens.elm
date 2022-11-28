module Lens exposing (..)

import Base exposing (Lens, Optic, lens)


bar : Lens ls a a x y -> Lens ls { m | bar : a } { m | bar : a } x y
bar =
    lens ".bar" .bar (\rec val -> { rec | bar = val })


foo : Lens ls a a x y -> Lens ls { m | foo : a } { m | foo : a } x y
foo =
    lens ".foo" .foo (\rec val -> { rec | foo = val })


qux : Lens ls a a x y -> Lens ls { m | qux : a } { m | qux : a } x y
qux =
    lens ".qux" .qux (\rec val -> { rec | qux = val })


name : Lens ls a a x y -> Lens ls { m | name : a } { m | name : a } x y
name =
    lens ".name" .name (\rec val -> { rec | name = val })


age : Lens ls a a x y -> Lens ls { m | age : a } { m | age : a } x y
age =
    lens ".age" .age (\rec val -> { rec | age = val })


email : Lens ls a a x y -> Lens ls { m | email : a } { m | email : a } x y
email =
    lens ".email" .email (\rec val -> { rec | email = val })


stuff : Lens ls a a x y -> Lens ls { m | stuff : a } { m | stuff : a } x y
stuff =
    lens ".stuff" .stuff (\rec val -> { rec | stuff = val })


things : Lens ls a a x y -> Lens ls { m | things : a } { m | things : a } x y
things =
    lens ".things" .things (\rec val -> { rec | things = val })


info : Lens ls a a x y -> Lens ls { m | info : a } { m | info : a } x y
info =
    lens ".info" .info (\rec val -> { rec | info = val })
