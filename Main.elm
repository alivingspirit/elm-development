module Main (..) where

import GuessField
import Html
import Time
import Html.Events exposing (onClick)
import Random


type Action
  = NewGame
  | GuessFieldAction GuessField.Action


type GameState
  = Started
  | Finished


type alias Model =
  { gameState : GameState
  , guessField : GuessField.Model
  , seed : Maybe.Maybe Random.Seed
  }


init : () -> Model
init () =
  Model Finished (GuessField.init 0) Maybe.Nothing


randomIntGenerator : Random.Generator Int
randomIntGenerator =
  Random.int 1 100


mailbox : Signal.Mailbox Action
mailbox =
  Signal.mailbox NewGame


guessFieldAddress : Signal.Address GuessField.Action
guessFieldAddress =
  Signal.forwardTo mailbox.address GuessFieldAction


update : ( Time.Time, Action ) -> Model -> Model
update ( timestamp, action ) model =
  case action of
    GuessFieldAction action ->
      { model | guessField = GuessField.update action model.guessField }

    NewGame ->
      let
        time =
          timestamp |> round

        oldSeed =
          case model.seed of
            Nothing ->
              Random.initialSeed time

            Just seed ->
              seed

        ( randomNumber, newSeed ) =
          Random.generate randomIntGenerator oldSeed
      in
        { model | guessField = GuessField.init randomNumber, gameState = Started, seed = Just newSeed }


model : Signal Model
model =
  Signal.foldp update (init ()) (Time.timestamp mailbox.signal)


view : Model -> Html.Html
view model =
  let
    appView =
      case model.gameState of
        Finished ->
          Html.button [ onClick mailbox.address NewGame ] [ Html.text "New Game" ]

        Started ->
          GuessField.view guessFieldAddress model.guessField
  in
    Html.div [] [ appView ]


main : Signal Html.Html
main =
  Signal.map view model
