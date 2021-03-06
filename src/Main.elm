module Main exposing (main)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Random
import Time


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Status
    = Completed
    | Incomplete


type alias Task =
    { title : String
    , position : Int
    , numOfPomos : Int
    }


type StartingTime
    = Regular
    | ShortBreak
    | LongBreak


type alias Model =
    { taskList : List Task
    , content : String
    , taskMap : Dict.Dict Int Status
    , currentTime : Int
    , clockStarted : Bool
    , totalNumOfPomos : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { taskList = [], content = "", taskMap = Dict.fromList [], currentTime = setCurrentTime Regular, clockStarted = False, totalNumOfPomos = 0 }
    , Cmd.none
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
        Completed ->
            Incomplete

        Incomplete ->
            Completed


setCurrentTime duration =
    case duration of
        ShortBreak ->
            1 * 3

        LongBreak ->
            15 * 60

        Regular ->
            25 * 60


getTaskValue k dict =
    case Dict.get k dict of
        Nothing ->
            Incomplete

        Just v ->
            v


addNewTask model task =
    List.append model.taskList [ { title = task, position = List.length model.taskList, numOfPomos = 0 } ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            if key == 13 then
                ( { model | taskList = addNewTask model model.content }, Cmd.none )

            else
                ( model, Cmd.none )

        ChangeStartTime duration ->
            ( { model | currentTime = setCurrentTime duration, clockStarted = False }, Cmd.none )

        StartClock hasStarted ->
            ( { model | clockStarted = hasStarted }, Cmd.none )

        AddNewTask task ->
            ( { model | taskList = addNewTask model task }
            , Cmd.none
            )

        UpdateContent newContent ->
            ( { model
                | content = newContent
              }
            , Cmd.none
            )

        UpdateTaskCompletion task ->
            ( { model
                | taskMap = Dict.insert task.position (toggle (getTaskValue task.position model.taskMap)) model.taskMap
              }
            , Cmd.none
            )

        Tick time ->
            if model.clockStarted then
                if model.currentTime == 0 then
                    ( { model | taskList = applyAddOne model.taskList model.taskMap, totalNumOfPomos = model.totalNumOfPomos + 1, clockStarted = False }, Cmd.none )

                else
                    ( { model | currentTime = model.currentTime - 1 }, Cmd.none )

            else
                ( model, Cmd.none )


addOne : Dict.Dict Int Status -> Task -> Task
addOne taskMap task =
    if checkStatus taskMap True task then
        { task | numOfPomos = task.numOfPomos + 1 }

    else
        task


applyAddOne : List Task -> Dict.Dict Int Status -> List Task
applyAddOne taskList taskMap =
    List.map (addOne taskMap) taskList


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


statusToBool s =
    case s of
        Incomplete ->
            False

        Completed ->
            True


negate shouldNegate status =
    if shouldNegate then
        not status

    else
        status


checkStatus : Dict.Dict Int Status -> Bool -> Task -> Bool
checkStatus tasksCompleted shouldNegate task =
    Dict.get task.position tasksCompleted |> Maybe.withDefault Incomplete |> statusToBool |> negate shouldNegate


printTasks taskList acc taskMap totalPomos =
    case taskList of
        [] ->
            acc

        task :: rest ->
            printTasks rest
                (List.append acc
                    [ div []
                        [ input
                            [ type_ "checkbox"
                            , checked (checkStatus taskMap False task)
                            , onClick (UpdateTaskCompletion task)
                            ]
                            []
                        , text task.title
                        , text (" " ++ String.fromInt task.numOfPomos ++ "/" ++ String.fromInt totalPomos)
                        ]
                    ]
                )
                taskMap
                totalPomos


getMinutesFromTime time =
    time // 60


getSecondsFromTime time =
    let
        seconds =
            String.fromInt (modBy 60 time)
    in
    if String.length seconds == 1 then
        "0" ++ seconds

    else
        seconds


onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


view : Model -> Html Msg
view model =
    let
        minute =
            String.fromInt (getMinutesFromTime model.currentTime)

        seconds =
            getSecondsFromTime model.currentTime
    in
    div []
        [ button [ onClick (ChangeStartTime LongBreak) ] [ text "Long Break" ]
        , button [ onClick (ChangeStartTime Regular) ] [ text "Pomodoro" ]
        , button [ onClick (ChangeStartTime ShortBreak) ] [ text "Short Break" ]
        , button [ onClick (StartClock (not model.clockStarted)) ]
            [ text
                (if model.clockStarted then
                    "Stop Timer"

                 else
                    "Start Timer"
                )
            ]
        , div [] [ text (minute ++ ":" ++ seconds) ]
        , input [ placeholder "Add New Task", value model.content, onInput UpdateContent, onKeyDown KeyDown ] []
        , button [ onClick (AddNewTask model.content) ] [ text "Add New Task" ]
        , div [] [ text "Current Tasks" ]
        , div [] (printTasks (List.filter (checkStatus model.taskMap True) model.taskList) [ div [] [] ] model.taskMap model.totalNumOfPomos)
        , div [] [ text "Completed Tasks" ]
        , div [] (printTasks (List.filter (checkStatus model.taskMap False) model.taskList) [ div [] [] ] model.taskMap model.totalNumOfPomos)
        ]
