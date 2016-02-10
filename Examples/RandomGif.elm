import Effects exposing (Effects)
import Task
import Http
import Json.Decode exposing (Decoder)

(=>) : a -> b -> ( a, b )
(=>) = (,)

type alias Model
  = {
    topic: String,
    gifUrl: String
  }

type Action
  = RequestMore
  | NewGif (Maybe String)

update: Action -> Model -> (Model, Effects Action)
update msg model =
  case msg of
    RequestMore ->
      (model, getRandomGif model.topic)
    NewGif maybeString ->
        (Model model.topic (Maybe.withDefault model.gifUrl maybeString)
        ,Effects.none)

getRandomGif : String -> Effects Action
getRandomGif topic =
  Http.get decodeImageUrl (randomUrl topic)
    |> Task.toMaybe
    |> Task.map NewGif
    |> Effects.task

-- The first line there created an HTTP GET request. It tries to
-- get some JSON at `randomUrl topic` and decodes the result
-- with `decodeImageUrl`. Both are defined below!
--
-- Next we use `Task.toMaybe` to capture any potential failures and
-- apply the `NewGif` tag to turn the result into a `Action`.
-- Finally we turn it into an `Effects` value that can be used in our
-- `init` or `update` functions.


-- Given a topic, construct a URL for the giphy API.
randomUrl : String -> String
randomUrl topic =
  Http.url "http://api.giphy.com/v1/gifs/random"
    [ "api_key" => "dc6zaTOxFJmzC"
    , "tag" => topic
    ]


-- A JSON decoder that takes a big chunk of JSON spit out by
-- giphy and extracts the string at `json.data.image_url`
decodeImageUrl : Decoder String
decodeImageUrl =
  Json.Decode.at ["data", "image_url"] Json.Decode.string
