module SpaceGame.Star (..) where

import Graphics.Element exposing (show, Element)
import Graphics.Collage exposing (collage, move, Form)
import Array exposing (Array, fromList)
import Color exposing (Color, rgb, hsl)
import Random exposing (Generator)


type alias Range a =
  { min : a
  , max : a
  }


type alias Layer =
  { percent : Int
  , size : Range Float
  , speed : Range Int
  , colorGenerator : Generator Color
  }


bottomLayer : Layer
bottomLayer =
  { percent = 30, size = { min = 0.4, max = 1.0 }, speed = { min = 1, max = 2 }, colorGenerator = colorGeneratorFromList [ hsl 0 0 7, hsl 0 0 7, hsl 0 78 30 ] }


layers : List Layer
layers =
  [ bottomLayer
  , { percent = 25, size = { min = 0.6, max = 1.2 }, speed = { min = 2, max = 4 }, colorGenerator = colorGeneratorFromList [ hsl 0 0 20, hsl 0 0 20, hsl 0 45 37 ] }
  , { percent = 15, size = { min = 0.8, max = 1.4 }, speed = { min = 4, max = 8 }, colorGenerator = colorGeneratorFromList [ hsl 0 0 33, hsl 0 0 33, hsl 0 23 43 ] }
  , { percent = 15, size = { min = 1.0, max = 1.6 }, speed = { min = 8, max = 16 }, colorGenerator = colorGeneratorFromList [ hsl 0 0 47 ] }
  , { percent = 8, size = { min = 1.2, max = 1.8 }, speed = { min = 16, max = 32 }, colorGenerator = colorGeneratorFromList [ hsl 0 0 60 ] }
  , { percent = 4, size = { min = 1.4, max = 2.0 }, speed = { min = 32, max = 64 }, colorGenerator = colorGeneratorFromList [ hsl 0 0 73 ] }
  , { percent = 2, size = { min = 1.6, max = 2.2 }, speed = { min = 64, max = 128 }, colorGenerator = colorGeneratorFromList [ hsl 0 0 87 ] }
  , { percent = 1, size = { min = 1.8, max = 2.4 }, speed = { min = 128, max = 256 }, colorGenerator = colorGeneratorFromList [ hsl 0 0 100 ] }
  ]


colorGeneratorFromList : List Color -> Generator Color
colorGeneratorFromList list =
  case list of
    [] ->
      Debug.crash "Please define colors when creating a layer"

    [ head ] ->
      Random.int 0 0 |> Random.map (always head)

    head :: rest ->
      let
        colors =
          Array.fromList list

        length =
          Array.length colors

        lenghtIndex =
          length - 1
      in
        Random.int 0 lenghtIndex |> Random.map (picker colors head)


picker : Array a -> a -> Int -> a
picker array default i =
  (Maybe.withDefault default (Array.get i array))


type alias Model =
  { x : Float
  , y : Float
  , size : Float
  , speed : Int
  , color : Color
  }


randomLayer : Generator Layer
randomLayer =
  Random.map (getLayerFromPercentage layers) (Random.int 1 100)


getLayerFromPercentage : List Layer -> Int -> Layer
getLayerFromPercentage layers i =
  case layers of
    [] ->
      bottomLayer

    first :: rest ->
      let
        i' =
          i - first.percent
      in
        if i' <= 0 then
          first
        else
          getLayerFromPercentage rest i'


randomStarFromLayer : { x : Int, y : Int } -> Layer -> Generator Model
randomStarFromLayer windowSize layer =
  let
    windowX =
      windowSize.x // 2

    windowY =
      windowSize.y // 2

    randomX =
      Random.int (-windowX) windowX

    randomY =
      Random.int (-windowY) windowY

    randomSize =
      Random.float layer.size.min layer.size.max

    randomSpeed =
      Random.int layer.speed.min layer.speed.max

    randomColor =
      layer.colorGenerator
  in
    Random.map5 (\x y size speed color -> Model (toFloat x) (toFloat y) size speed color) randomX randomY randomSize randomSpeed randomColor


randomStar : { x : Int, y : Int } -> Generator Model
randomStar window =
  Random.andThen randomLayer (randomStarFromLayer window)


view : Model -> Form
view star =
  Graphics.Collage.circle star.size |> Graphics.Collage.filled star.color |> move ( star.x, star.y )


background : { a | x : Int, y : Int } -> Form
background window =
  Graphics.Collage.rect (toFloat window.x) (toFloat window.y) |> Graphics.Collage.filled Color.black


main : Element
main =
  let
    window =
      { x = 800, y = 600 }

    seed =
      Random.initialSeed 12334253
  in
    collage
      window.x
      window.y
      (background window
        :: (seed
              |> Random.generate (Random.list 1000 (randomStar window))
              |> fst
              |> List.map view
           )
      )
