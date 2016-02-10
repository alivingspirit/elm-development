module SpaceGame.Player (..) where

import Utilities.Tuple as Tuple


type alias Model =
  { x : Float
  , y : Float
  , dx : Float
  , dy : Float
  }


init : Model
init =
  Model 0 0 0 0


velocityMultiplier : Float
velocityMultiplier =
  0.1


update : ( Int, Int ) -> Model -> Model
update ( xmult, ymult ) model =
  let
    ( xmult', ymult' ) =
      ( xmult, ymult ) |> Tuple.map toFloat

    newDx =
      model.dx + (xmult' * velocityMultiplier)

    newDy =
      model.dy + (ymult' * velocityMultiplier)

    newX =
      model.x + newDx

    newY =
      model.y + newDy
  in
    { model | x = newX, y = newY, dx = newDx, dy = newDy }
