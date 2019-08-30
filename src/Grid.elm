module Grid exposing (Model, Msg, init, mainGrid, update, view)

import Html exposing (Html, div, table, text, th, tr)
import Utils exposing (schoolDays, timeSlots)


type alias Model =
    { grid : Html Msg
    , gridHeader : Html Msg
    , gridTimeSlots : Html Msg
    }


type Msg
    = None


init : Model
init =
    { grid = mainGridHeader
    , gridHeader = mainGridHeader
    , gridTimeSlots = mainGridTimeSlots
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ model.grid ]


mainGridHeader : Html Msg
mainGridHeader =
    tr []
        (List.append
            [ tr [] [ text "" ] ]
            (List.map
                (\d -> th [] [ text d ])
                schoolDays
            )
        )


mainGridTimeSlots : Html Msg
mainGridTimeSlots =
    tr [] (List.map (\t -> tr [] [ text t ]) timeSlots)


mainGrid : Html Msg
mainGrid =
    table []
        [ mainGridHeader
        , mainGridTimeSlots
        ]
