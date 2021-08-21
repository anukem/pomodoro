module Main exposing (main)
import Browser
import Dict
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Html.Attributes exposing (..)
import Time
import Json.Decode as Json

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

type StartingTime = Regular | ShortBreak | LongBreak

type alias Model =
  { taskList : List Task,
    content : String,
    taskMap : Dict.Dict Int Status,
    currentTime : Int,
    clockStarted : Bool
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( {taskList = [], content = "", taskMap = Dict.fromList [], currentTime = (setCurrentTime Regular), clockStarted = False},
  Cmd.none
  )

type Msg
  = AddNewTask String
  | UpdateContent String
  | UpdateTaskCompletion Task
  | StartClock Bool
  | Tick Time.Posix
  | ChangeStartTime StartingTime
  | KeyDown Int

toggle s =
  case s of
    Completed -> Incomplete
    Incomplete -> Completed

setCurrentTime duration =
  case duration of
    ShortBreak -> 5 * 60
    LongBreak -> 15 * 60
    Regular -> 25 * 60

getTaskValue k dict =
  case Dict.get k dict of
    Nothing -> Incomplete
    Just v -> v

addNewTask model task = List.append model.taskList [{title = task, position = (List.length model.taskList)}]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyDown key ->
      if key == 13 then ({ model | taskList = addNewTask model model.content }, Cmd.none) else (model, Cmd.none)
    ChangeStartTime duration ->
      ({model | currentTime = (setCurrentTime duration), clockStarted = False }, Cmd.none)
    StartClock hasStarted -> ({ model | clockStarted = hasStarted }, Cmd.none)
    AddNewTask task->
      ( { model | taskList = (addNewTask model task)  },
        Cmd.none
      )
    UpdateContent newContent ->
      ({
        model | content = newContent
      }, Cmd.none)
    UpdateTaskCompletion task ->
      ({
      model | taskMap = Dict.insert task.position (toggle (getTaskValue task.position model.taskMap)) model.taskMap
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

checkStatus : Dict.Dict Int Status -> Task -> Bool
checkStatus tasksCompleted task  =
  Dict.get task.position tasksCompleted |> Maybe.withDefault Incomplete |> statusToBool

printTasks taskList acc taskMap =
  case taskList of
    [] -> acc
    (task::rest) -> printTasks rest (List.append acc [div [] [ input [ type_ "checkbox", checked (checkStatus taskMap task), onClick (UpdateTaskCompletion task)] [] , text task.title]]) taskMap

getMinutesFromTime time = time // 60

getSecondsFromTime time =
  let seconds = String.fromInt (modBy 60 time) in
  if String.length seconds == 1 then "0" ++ seconds else seconds


onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode)

view : Model -> Html Msg
view model =
  let
    minute = String.fromInt (getMinutesFromTime model.currentTime)
    seconds = getSecondsFromTime model.currentTime
  in

  div []
    [
     button [ onClick (ChangeStartTime LongBreak )  ] [ text "Long Break" ],
     button [ onClick (ChangeStartTime Regular )  ] [ text "Pomodoro" ],
     button [ onClick (ChangeStartTime ShortBreak )  ] [ text "Short Break" ],
     button [ onClick (StartClock (not model.clockStarted))  ] [ text ( if model.clockStarted then "Stop Timer" else  "Start Timer") ],
     div [] [text (minute ++ ":" ++ seconds) ],
     input [placeholder "Add New Task", value model.content, onInput UpdateContent, onKeyDown KeyDown ] [ ],
     button [ onClick (AddNewTask model.content) ] [ text "Add New Task" ],
     div [  ] [text "Current Tasks"],
     div []  (printTasks (List.filter  (checkStatus model.taskMap) model.taskList) [div [] []] model.taskMap),
     div [  ] [text "Completed Tasks"]
     ]
