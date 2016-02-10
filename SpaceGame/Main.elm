module SpaceGame.Main (..) where

import SpaceGame.Player as Player
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window


type alias Input =
  { keyboardX : Int
  , keyboardY : Int
  }


type alias Model =
  { player : Player.Model
  }


init : Model
init =
  Model Player.init


inputSignal : Signal Input
inputSignal =
  Signal.map (\key -> Input key.x key.y) Keyboard.arrows


timedInputSignal : Signal Input
timedInputSignal =
  Signal.sampleOn (Time.fps 35) inputSignal


model : Signal Model
model =
  Signal.foldp update init timedInputSignal


update : Input -> Model -> Model
update input model =
  let
    newPlayer =
      Player.update ( input.keyboardX, input.keyboardY ) model.player
  in
    { model | player = newPlayer }


main : Signal Element
main =
  Signal.map show model
