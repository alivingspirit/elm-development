module Utilities.Random (..) where

import Random


type alias GeneratedValue a =
  { value : a
  , seed : Random.Seed
  }


generate : Random.Generator a -> Random.Seed -> GeneratedValue a
generate generator seed =
  Random.generate generator seed |> uncurry GeneratedValue
