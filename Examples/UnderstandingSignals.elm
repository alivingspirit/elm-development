import Graphics.Element exposing (show, Element)
import Mouse
import Signal exposing (map, foldp)
import Time exposing (Time)

type alias Model =
  { clicks: Int
  , time: Time
  }

model: Model
model =  {clicks = 0, time = 0}

type Event = MouseClick Time

update: Event -> Model -> Model
update event model =
  case event of
    MouseClick t-> {model | clicks = model.clicks + 1, time = t}

view: Model -> Element
view model = show model

clickEvent: Signal Event
clickEvent = Signal.map (\(time, ()) -> MouseClick time) (Time.timestamp Mouse.clicks)

main: Signal Element
main = map view (foldp update model clickEvent)
