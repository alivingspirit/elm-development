module SpaceGame.Main (..) where

import SpaceGame.Player as Player
import SpaceGame.StarField as StarField
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window


type alias Input =
  { x : Int
  , y : Int
  , delta : Int
  , time : Time.Time
  }


type alias Model =
  { player : Player.Model
  , starField : StarField.Model
  }


init : Model
init =
  Model Player.init


timedInputSignal : Signal Input
timedInputSignal =
  let
    delta =
      Time.fps 35

    deltaTimestamp =
      Time.timestamp delta

    time =
      Signal.map (\deltaTimestamp -> { time = fst deltaTimestamp, delta = snd deltaTimestamp })
  in
    Signal.sampleOn delta (Signal.map2 (\time input -> Model input.x input.y time.delta time.time) time Keyboard.arrows)


model : Signal Model
model =
  Signal.foldp update init timedInputSignal


update : Input -> Model -> Model
update input model =
  let
    newPlayer =
      Player.update ( input.keyboardX, input.keyboardY ) input.delta model.player
  in
    { model | player = newPlayer }


main : Signal Element
main =
  Signal.map show model
