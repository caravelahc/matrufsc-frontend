module Api exposing (ApiResponse(..), Class, ClassID, Course, CourseID, classDecoder, courseDecoder, fetchCampi, fetchClasses, fetchCourses, fetchSemesters, maybeParam)

import Http
import Json.Decode as D exposing (Decoder)
import Url.Builder exposing (QueryParameter, absolute, crossOrigin)


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
    D.map3
        Course
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "class_hours" D.int)


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


type alias Class =
    { id : ClassID
    , labels : List String
    , capacity : Int
    , enrolled : Int
    , special : Int
    , waiting : Maybe Int
    , times : List String
    , professors : List String
    }


classDecoder : Decoder Class
classDecoder =
    D.map8
        Class
        (D.field "class_id" D.string)
        (D.field "class_labels" (D.list D.string))
        (D.field "capacity" D.int)
        (D.field "enrolled" D.int)
        (D.field "special" D.int)
        (D.field "waiting" (D.nullable D.int))
        (D.field "times" (D.list D.string))
        (D.field "professors" (D.list D.string))


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
