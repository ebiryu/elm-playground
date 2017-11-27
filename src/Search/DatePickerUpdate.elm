module Search.DatePickerUpdate exposing (..)

import Date exposing (Date)
import Date.Extra.Duration as Duration
import Date.Extra.Field as Field


type Msg
    = Noop
    | NextMonth
    | PrevMonth
    | ClickDay Int


type alias Model =
    { date : Date
    , dateNow : Date
    , check : Check
    }


initDatePicker : a -> { check : Check, date : a, dateNow : a }
initDatePicker date =
    { date = date
    , dateNow = date
    , check = CheckIn
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model

        NextMonth ->
            { model | date = Duration.add Duration.Month 1 model.date }

        PrevMonth ->
            { model | date = Duration.add Duration.Month -1 model.date }

        ClickDay int ->
            { model
                | date =
                    Field.fieldToDate (Field.DayOfMonth int) model.date
                        |> Maybe.withDefault model.date
            }


type Check
    = CheckIn
    | CheckOut
