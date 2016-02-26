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
import Random
import Utilities.Random as URandom


type alias Input =
  { x : Int
  , y : Int
  , delta : Float
  , time : Time.Time
  }


type alias Model =
  { player : Player.Model
  , starField : StarField.Model
  }


window =
  { x = 1200, y = 600 }


init : Model
init =
  let
    starFieldGenerator =
      StarField.random window

    seed =
      Random.initialSeed 100

    generatedValue =
      URandom.generate starFieldGenerator seed
  in
    Model Player.init generatedValue.value


timedInputSignal : Signal Input
timedInputSignal =
  let
    delta =
      Time.fps 35

    deltaTimestamp =
      Time.timestamp delta

    time =
      Signal.map (\deltaTimestamp -> { time = fst deltaTimestamp, delta = snd deltaTimestamp }) deltaTimestamp
  in
    Signal.sampleOn delta (Signal.map2 (\time input -> Input input.x input.y time.delta time.time) time Keyboard.arrows)


model : Signal Model
model =
  Signal.foldp update init timedInputSignal


update : Input -> Model -> Model
update input model =
  let
    newPlayer =
      Player.update input model.player

    newStarField =
      StarField.update newPlayer window model.starField
  in
    { model | player = newPlayer, starField = newStarField }


view : Model -> Element
view model =
  StarField.view window model.starField


main : Signal Element
main =
  Signal.map view model
