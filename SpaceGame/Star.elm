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
