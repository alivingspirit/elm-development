module Counter where

import Html exposing (div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

type alias Model = Int

type Action = Increment | Decrement

init: Int -> Model
init count = count

update: Action -> Model -> Model
update action model =
  case action of
    Increment -> model + 1
    Decrement -> model - 1

type alias Context = {
  action: Signal.Address Action,
  remove: Signal.Address ()
}

view: Context -> Model -> Html.Html
view context model =
  div [] [
    button [onClick context.action Decrement] [text "-"],
    div [counterStyle] [text (toString model)],
    button [onClick context.action Increment] [text "+"],
    button [onClick context.remove ()] [text "X"]
  ]

counterStyle : Html.Attribute
counterStyle =
  style
      [ ("font-size", "20px")
      , ("font-family", "monospace")
      , ("display", "inline-block")
      , ("width", "50px")
      , ("text-align", "center")
      ]
