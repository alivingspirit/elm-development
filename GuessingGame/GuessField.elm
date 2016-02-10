module GuessingGame.GuessField (..) where

import Html
import Html.Attributes exposing (style)
import Html.Events exposing (targetValue, on)
import String


(=>) : a -> b -> ( a, b )
(=>) =
  (,)


type alias Model =
  { guess : Guess
  , answer : Int
  }


type Guess
  = GuessNumber Int
  | Error String


type Action
  = GuessChanged Guess


init : Int -> Model
init int =
  { guess = GuessNumber 0
  , answer = int
  }


update : Action -> Model -> Model
update action model =
  if isAnswered model then
    model
  else
    case action of
      GuessChanged i ->
        { model | guess = i }


view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    s =
      case model.guess of
        GuessNumber int ->
          if int > model.answer then
            "Too High"
          else if int < model.answer then
            "Too Low"
          else
            "Correct"

        Error error ->
          ""
  in
    Html.div
      []
      [ Html.input ([ on "input" targetValue (makeMessage address) ] ++ (colorAttribute model)) []
      , Html.div [] [ Html.text s ]
      ]


stringToAction : String -> Action
stringToAction string =
  let
    result =
      String.toInt string
  in
    case result of
      Ok int ->
        GuessChanged (GuessNumber int)

      Err err ->
        GuessChanged (Error err)


makeMessage : Signal.Address Action -> String -> Signal.Message
makeMessage address string =
  Signal.message address (stringToAction string)


isAnswered : Model -> Bool
isAnswered model =
  case model.guess of
    GuessNumber guess ->
      if guess == model.answer then
        True
      else
        False

    _ ->
      False


colorAttribute : Model -> List Html.Attribute
colorAttribute model =
  if isAnswered model then
    [ style [ "background-color" => "green" ] ]
  else
    []
