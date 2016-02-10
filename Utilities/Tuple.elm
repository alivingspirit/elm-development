module Utilities.Tuple (..) where


map : (a -> b) -> ( a, a ) -> ( b, b )
map fun ( a, b ) =
  ( fun a, fun b )
