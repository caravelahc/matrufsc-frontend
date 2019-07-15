module Main exposing (Model, Msg(..), init, main, update, view)

import Api exposing (ApiResponse(..), Class, Course, fetchCourses)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.element
        { init = \() -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Model =
    { semester : Maybe String
    , campus : Maybe String
    , courses : List Course
    , classes : List Class
    , selectedClass : Maybe Class
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        Nothing
        Nothing
        []
        []
        Nothing
    , Cmd.map (\c -> ApiResponse c) (fetchCourses (Just "FLO") (Just "20192"))
    )


type Msg
    = ApiResponse ApiResponse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApiResponse response ->
            case response of
                GotCourses result ->
                    ( { model | courses = Result.withDefault [] result }, Cmd.none )

                GotClasses result ->
                    ( { model | classes = Result.withDefault [] result }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ input [ list "courses" ] []
        , datalist [ id "courses" ] <|
            List.map
                (\c -> option [ value (c.id ++ " - " ++ c.name) ] [])
                model.courses
        ]
