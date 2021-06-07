module Main exposing (main)
import Browser
import Dict
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Html.Attributes exposing (..)

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type Status = Completed | Incomplete

type alias Model =
  { taskList : List String,
    content : String,
    completedTasks : Dict.Dict String Status
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( {taskList = [], content = "", completedTasks = Dict.fromList [] },
  Cmd.none
  )

type Msg
  = AddNewTask String
  | UpdateContent String
  | UpdateTaskCompletion String

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
    AddNewTask task->
      ( { model | taskList = List.append model.taskList [task] },
        Cmd.none
      )
    UpdateContent newContent ->
      ({
        model | content = newContent
      }, Cmd.none)
    UpdateTaskCompletion task ->
      ({
      model | completedTasks = Dict.insert task (toggle (getTaskValue task model.completedTasks)) model.completedTasks
      }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


statusToBool s =
  case s of
  Incomplete -> False
  Completed -> True

checkStatus task tasksCompleted =
  Dict.get task tasksCompleted |> Maybe.withDefault Incomplete |> statusToBool

printTasks taskList acc completedTasks =
  case taskList of
    [] -> acc
    (h::t) -> printTasks t (List.append acc [div [] [ input [ type_ "checkbox", checked (checkStatus h completedTasks), onClick (UpdateTaskCompletion h)] [] , text h]]) completedTasks

view : Model -> Html Msg
view model =
  div []
    [
     input [placeholder "Add New Task", value model.content, onInput UpdateContent ] [ ],
     button [ onClick (AddNewTask model.content) ] [ text "Add New Task" ], div []  (printTasks model.taskList [div [] []] model.completedTasks)
    ]
