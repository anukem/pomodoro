module Main exposing (main)
import Browser
import Dict
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Html.Attributes exposing (..)
import Time

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type Status = Completed | Incomplete

type alias Task =
  {
  title : String,
  position : Int
  }

type alias Model =
  { taskList : List Task,
    content : String,
    completedTasks : Dict.Dict Int Status,
    currentTime : Int,
    clockStarted : Bool
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( {taskList = [], content = "", completedTasks = Dict.fromList [], currentTime = 1 * 10, clockStarted = False},
  Cmd.none
  )

type Msg
  = AddNewTask String
  | UpdateContent String
  | UpdateTaskCompletion Task
  | StartClock Bool
  | Tick Time.Posix

toggle s =
  case s of
    Completed -> Incomplete
    Incomplete -> Completed

getTaskValue k dict =
  case Dict.get k dict of
    Nothing -> Incomplete
    Just v -> v

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartClock hasStarted -> ({ model | clockStarted = hasStarted }, Cmd.none)
    AddNewTask task->
      ( { model | taskList = List.append model.taskList [{title = task, position = (List.length model.taskList)}] },
        Cmd.none
      )
    UpdateContent newContent ->
      ({
        model | content = newContent
      }, Cmd.none)
    UpdateTaskCompletion task ->
      ({
      model | completedTasks = Dict.insert task.position (toggle (getTaskValue task.position model.completedTasks)) model.completedTasks
      }, Cmd.none)
    Tick time ->
      if model.clockStarted && model.currentTime /= 0 then
      ({ model | currentTime = model.currentTime - 1 }, Cmd.none) else (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Time.every 1000 Tick

statusToBool s =
  case s of
  Incomplete -> False
  Completed -> True

checkStatus task tasksCompleted =
  Dict.get task tasksCompleted |> Maybe.withDefault Incomplete |> statusToBool

printTasks taskList acc completedTasks =
  case taskList of
    [] -> acc
    (task::rest) -> printTasks rest (List.append acc [div [] [ input [ type_ "checkbox", checked (checkStatus task.position completedTasks), onClick (UpdateTaskCompletion task)] [] , text task.title]]) completedTasks

getMinutesFromTime time = time // 60

getSecondsFromTime time =
  let seconds = String.fromInt (modBy 60 time) in
  if String.length seconds == 1 then "0" ++ seconds else seconds

view : Model -> Html Msg
view model =
  let
    minute = String.fromInt (getMinutesFromTime model.currentTime)
    seconds = getSecondsFromTime model.currentTime
  in

  div []
    [
     button [ onClick (StartClock (not model.clockStarted))  ] [ text ( if model.clockStarted then "Stop Timer" else  "Start Timer") ],
     div [] [text (minute ++ ":" ++ seconds) ],
     input [placeholder "Add New Task", value model.content, onInput UpdateContent ] [ ],
     button [ onClick (AddNewTask model.content) ] [ text "Add New Task" ],
     div []  (printTasks model.taskList [div [] []] model.completedTasks)
     ]
