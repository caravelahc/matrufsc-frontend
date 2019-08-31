module Api exposing
    ( ApiResponse(..)
    , Class
    , ClassID
    , Course
    , CourseID
    , Slot(..)
    , SlotParseError(..)
    , TimePlace
    , WeekDay(..)
    , WeekDayParseError(..)
    , classDecoder
    , courseDecoder
    , endpointUrl
    , fetchCampi
    , fetchClasses
    , fetchCourses
    , fetchSemesters
    , maybeParam
    , slotDecoder
    , slotFromString
    , timePlaceDecoder
    , weekDayDecoder
    , weekDayFromInt
    )

import Http
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D exposing (required)
import Url.Builder exposing (QueryParameter, crossOrigin)


endpointUrl : String -> List QueryParameter -> String
endpointUrl endpoint parameters =
    -- absolute [ endpoint ] parameters
    crossOrigin "http://localhost:8080" [ endpoint ] parameters


type ApiResponse
    = GotSemesters (Result Http.Error (List String))
    | GotCampi (Result Http.Error (List String))
    | GotCourses (Result Http.Error (List Course))
    | GotClasses (Result Http.Error (List Class))


fetchSemesters : Cmd ApiResponse
fetchSemesters =
    Http.get
        { url = endpointUrl "semesters" []
        , expect = Http.expectJson GotSemesters (D.list D.string)
        }


fetchCampi : Cmd ApiResponse
fetchCampi =
    Http.get
        { url = endpointUrl "campi" []
        , expect = Http.expectJson GotCampi (D.list D.string)
        }


maybeParam : String -> Maybe String -> List QueryParameter
maybeParam key value =
    case value of
        Just v ->
            [ Url.Builder.string key v ]

        Nothing ->
            []


type alias CourseID =
    String


type alias Course =
    { id : CourseID
    , name : String
    , classHours : Int
    }


courseDecoder : Decoder Course
courseDecoder =
    D.succeed Course
        |> required "id" D.string
        |> required "name" D.string
        |> required "class_hours" D.int


fetchCourses : Maybe String -> Maybe String -> Cmd ApiResponse
fetchCourses semester campus =
    let
        parameters =
            maybeParam "semester" semester ++ maybeParam "campus" campus
    in
    Http.get
        { url = endpointUrl "courses" parameters
        , expect = Http.expectJson GotCourses (D.list courseDecoder)
        }


type alias ClassID =
    String


type WeekDay
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday


type WeekDayParseError
    = WeekDayOutOfRange


weekDayFromInt : Int -> Result WeekDayParseError WeekDay
weekDayFromInt i =
    case i of
        0 ->
            Ok Monday

        1 ->
            Ok Tuesday

        2 ->
            Ok Wednesday

        3 ->
            Ok Thursday

        4 ->
            Ok Friday

        5 ->
            Ok Saturday

        6 ->
            Ok Sunday

        _ ->
            Err WeekDayOutOfRange


weekDayDecoder : Decoder WeekDay
weekDayDecoder =
    D.andThen
        (\i ->
            case weekDayFromInt i of
                Ok w ->
                    D.succeed w

                Err WeekDayOutOfRange ->
                    D.fail ("Weekday out of range (" ++ String.fromInt i ++ ")")
        )
        D.int


type Slot
    = S0730
    | S0820
    | S0910
    | S1010
    | S1100
    | S1330
    | S1420
    | S1510
    | S1620
    | S1710
    | S1830
    | S1920
    | S2020
    | S2110


type SlotParseError
    = InvalidSlot


slotFromString : String -> Result SlotParseError Slot
slotFromString s =
    case s of
        "0730" ->
            Ok S0730

        "0820" ->
            Ok S0820

        "0910" ->
            Ok S0910

        "1010" ->
            Ok S1010

        "1100" ->
            Ok S1100

        "1330" ->
            Ok S1330

        "1420" ->
            Ok S1420

        "1510" ->
            Ok S1510

        "1620" ->
            Ok S1620

        "1710" ->
            Ok S1710

        "1830" ->
            Ok S1830

        "1920" ->
            Ok S1920

        "2020" ->
            Ok S2020

        "2110" ->
            Ok S2110

        _ ->
            Err InvalidSlot


slotDecoder : Decoder Slot
slotDecoder =
    D.andThen
        (\s ->
            case slotFromString s of
                Ok slot ->
                    D.succeed slot

                Err InvalidSlot ->
                    D.fail ("Invalid slot (" ++ s ++ ")")
        )
        D.string


type alias TimePlace =
    { weekday : WeekDay
    , slots : List Slot
    , room : String
    }


timePlaceDecoder : Decoder TimePlace
timePlaceDecoder =
    D.succeed TimePlace
        |> required "weekday" weekDayDecoder
        |> required "slots" (D.list slotDecoder)
        |> required "room" D.string


type alias Class =
    { id : ClassID
    , courseID : CourseID
    , labels : List String
    , capacity : Int
    , enrolled : Int
    , special : Int
    , waiting : Maybe Int
    , timesAndPlaces : List TimePlace
    , professors : List String
    }


classDecoder : Decoder Class
classDecoder =
    D.succeed Class
        |> required "id" D.string
        |> required "course_id" D.string
        |> required "labels" (D.list D.string)
        |> required "capacity" D.int
        |> required "enrolled" D.int
        |> required "special" D.int
        |> required "waiting" (D.nullable D.int)
        |> required "times_and_places" (D.list timePlaceDecoder)
        |> required "professors" (D.list D.string)


fetchClasses : CourseID -> Maybe String -> Cmd ApiResponse
fetchClasses courseId semester =
    let
        parameters =
            Url.Builder.string "course_id" courseId :: maybeParam "semester" semester
    in
    Http.get
        { url = endpointUrl "classes" parameters
        , expect = Http.expectJson GotClasses (D.list classDecoder)
        }
