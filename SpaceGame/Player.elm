module SpaceGame.Player (..) where

import Utilities.Tuple as Tuple


type alias Model =
  { x : Float
  , y : Float
  , dx : Float
  , dy : Float
  }


maxSpeed : Float
maxSpeed =
  1.0


init : Model
init =
  Model 0 0 0 0


velocityMultiplier : Float
velocityMultiplier =
  1.0e-2


clampFloat : Float -> Float -> Float -> Float
clampFloat min max n =
  if n < min then
    min
  else if n > max then
    max
  else
    n


update : { a | x : Int, y : Int } -> Model -> Model
update input model =
  let
    ( xmult', ymult' ) =
      ( input.x, input.y ) |> Tuple.map toFloat

    newDx =
      model.dx + (xmult' * velocityMultiplier)

    newDy =
      model.dy + (ymult' * velocityMultiplier)

    clamp' =
      clampFloat -maxSpeed maxSpeed

    newX =
      model.x + newDx |> clamp'

    newY =
      model.y + newDy |> clamp'
  in
    { model | x = newX, y = newY, dx = newDx, dy = newDy }
