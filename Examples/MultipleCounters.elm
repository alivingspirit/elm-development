import Counter
import Html
import Html.Events

type alias Model =
  {
    counters: List (ID,Counter.Model),
    nextId: ID
  }

type alias ID = Int

type Action = CounterAction ID Counter.Action | Remove ID | Add | Reset

init: Model
init =
  { counters = [], nextId = 0 }


update: Action -> Model -> Model
update action model =
  case action of
    Add ->
        let newCounter = (model.nextId, Counter.init 0)
        in
        {
          model |
            counters = model.counters ++ [newCounter],
            nextId = model.nextId + 1
        }
    Remove id ->
        let counterFilter (counterid, countermodel) = counterid /= id
        in
      {
        model |
          counters = List.filter counterFilter model.counters
        }
    CounterAction id action ->
      let updateCounter (counterid, countermodel) =
                                    if counterid == id
                                    then (counterid, Counter.update action countermodel)
                                    else (counterid, countermodel)
          newCounters = model.counters |> List.map updateCounter
      in
      {
        model |
          counters = newCounters
        }
    Reset -> init

view: Signal.Address Action -> Model -> Html.Html
view address model =
    let
      actionButton action text = Html.button [Html.Events.onClick address action] [Html.text text]
      counterSignal id = Signal.forwardTo address (CounterAction id)
      removeSignal id =  Signal.forwardTo address (always(Remove id))
      counterContext id = { action = counterSignal id, remove = removeSignal id }
      viewCounter (id, counter) = Html.span [] [Counter.view (counterContext id) counter]
    in
    Html.div []
             ([actionButton Reset "Reset",
              actionButton Add "Add"] ++ (model.counters |> List.map viewCounter)
                )
