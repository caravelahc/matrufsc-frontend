module Main exposing (Model, Msg(..), init, main, update, view)

import Api exposing (ApiResponse(..), Class, Course)
import Browser
import Html exposing (Html, datalist, div, input, option, p, select, table, td, text, th, tr)
import Html.Attributes exposing (disabled, id, list, value)
import Html.Events exposing (onInput)
import Platform exposing (Program)
import Utils exposing (classToOccupiedString, courseToString, schoolDays, selectableClassHeader, timePlaceListToString, timeSlots)


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Model =
    { availableSemesters : List String
    , selectedSemester : Maybe String
    , availableCampi : List String
    , selectedCampus : Maybe String
    , availableCourses : List Course
    , courseSearchAvailable : Bool
    , selectedCourse : Maybe Course
    , availableClasses : List Class
    , selectedClass : Maybe Class
    }


type Msg
    = GotApiResponse ApiResponse
    | SelectSemester String
    | SelectCampus String
    | ChangeCourseQuery String


init : ( Model, Cmd Msg )
init =
    ( { availableSemesters = []
      , selectedSemester = Nothing
      , availableCampi = []
      , selectedCampus = Nothing
      , availableCourses = []
      , courseSearchAvailable = True
      , selectedCourse = Nothing
      , availableClasses = []
      , selectedClass = Nothing
      }
    , Cmd.batch
        [ Cmd.map (\c -> GotApiResponse c) Api.fetchSemesters
        , Cmd.map (\c -> GotApiResponse c) Api.fetchCampi
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotApiResponse response ->
            case response of
                GotSemesters result ->
                    ( { model | availableSemesters = Result.withDefault [] result }
                    , Cmd.none
                    )

                GotCampi result ->
                    ( { model | availableCampi = Result.withDefault [] result }
                    , Cmd.none
                    )

                GotCourses result ->
                    ( { model
                        | availableCourses = Result.withDefault [] result
                        , courseSearchAvailable = True
                      }
                    , Cmd.none
                    )

                GotClasses result ->
                    ( { model | availableClasses = Result.withDefault [] result }
                    , Cmd.none
                    )

        SelectSemester s ->
            let
                semester =
                    if s == "" then
                        Nothing

                    else
                        Just s
            in
            ( { model
                | selectedSemester = semester
                , courseSearchAvailable = False
              }
            , Cmd.map GotApiResponse
                (Api.fetchCourses semester model.selectedCampus)
            )

        SelectCampus c ->
            let
                campus =
                    if c == "" then
                        Nothing

                    else
                        Just c
            in
            ( { model
                | selectedCampus = campus
                , courseSearchAvailable = False
              }
            , Cmd.map GotApiResponse
                (Api.fetchCourses model.selectedSemester campus)
            )

        ChangeCourseQuery query ->
            case
                List.head
                    (List.filter
                        (\c -> query == courseToString c)
                        model.availableCourses
                    )
            of
                Just course ->
                    ( { model | selectedCourse = Just course }
                    , Cmd.map GotApiResponse
                        (Api.fetchClasses course.id model.selectedSemester)
                    )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        opt s =
            option [ value s ] [ text s ]

        semesterSelector =
            select
                [ onInput SelectSemester
                , disabled (List.isEmpty model.availableSemesters)
                ]
                (List.map opt ("" :: model.availableSemesters))

        campusSelector =
            select
                [ onInput SelectCampus
                , disabled (List.isEmpty model.availableCampi)
                ]
                (List.map opt ("" :: model.availableCampi))

        courseSearchField =
            input
                [ list "courses"
                , disabled (not model.courseSearchAvailable)
                , onInput ChangeCourseQuery
                ]
                [ datalist [ id "courses" ]
                    (List.map (opt << courseToString) model.availableCourses)
                ]

        selectableClassesHeader =
            List.map (\header -> th [] [ text header ]) selectableClassHeader

        selectableClassesList =
            List.map
                (\class ->
                    tr []
                        [ td [] [ text class.id ]
                        , td [] [ text (classToOccupiedString class) ]
                        , td [] [ text (String.join "\n" class.professors) ]
                        , td [] [ text (timePlaceListToString class.times_and_places) ]
                        ]
                )
                model.availableClasses

        selectableClassesTable =
            table []
                (List.append
                    selectableClassesHeader
                    selectableClassesList
                )

        mainGridHeader =
            tr []
                (List.append
                    [ tr [] [ text "" ] ]
                    (List.map
                        (\d -> th [] [ text d ])
                        schoolDays
                    )
                )

        mainGridTimeSlots =
            tr [] (List.map (\t -> tr [] [ text t ]) timeSlots)

        mainGrid =
            table []
                [ mainGridHeader
                , mainGridTimeSlots
                ]
    in
    div [ id "main" ]
        [ p [] [ semesterSelector, campusSelector ]
        , courseSearchField
        , div [] [ mainGrid ]
        , div [] [ selectableClassesTable ]
        ]
