module SpaceGame.Star (..) where

import Graphics.Collage exposing (collage, move, Form, defaultLine, path)
import Color exposing (Color, rgb, hsl)


type alias Model =
  { x : Float
  , y : Float
  , size : Float
  , speed : Int
  , color : Color
  }


calculateFromRange : Float -> Float -> Float -> Float -> Float
calculateFromRange n dn speed minmax =
  let
    newN =
      n + (dn * speed)
  in
    if -minmax < newN && newN < minmax then
      newN
    else if newN < -minmax then
      newN + (minmax * 2)
    else
      newN - (minmax * 2)


update : { a | dx : Float, dy : Float } -> { b | x : Int, y : Int } -> Model -> Model
update diff window model =
  let
    windowX =
      window.x // 2

    windowY =
      window.y // 2

    newX =
      calculateFromRange model.x diff.dx (toFloat model.speed) (toFloat windowX)

    newY =
      calculateFromRange model.y diff.dy (toFloat model.speed) (toFloat windowY)
  in
    { model | x = newX, y = newY }



{-
view : Model -> Form
view star =
  Graphics.Collage.circle star.size |> Graphics.Collage.filled star.color |> move ( star.x, star.y )
-}


view : Model -> Form
view star =
  let
    linestyle =
      { defaultLine | width = star.size, color = star.color }

    dot =
      path [ ( 0, 0 ), ( 1, 1 ) ]
  in
    Graphics.Collage.traced linestyle dot
      |> move ( star.x, star.y )
