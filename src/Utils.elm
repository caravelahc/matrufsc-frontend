module Utils exposing
    ( classToOccupiedString
    , courseToString
    , schoolDays
    , selectableClassHeader
    , selectedCoursesHeaderList
    , timePlaceListToString
    , timeSlots
    )

import Api exposing (Class, Course, CourseID, TimePlace)
import List.Extra exposing (unique)


schoolDays : List String
schoolDays =
    [ "Segunda", "Terça", "Quarta", "Quinta", "Sexta", "Sábado" ]


timeSlots : List String
timeSlots =
    [ "07:30"
    , "08:20"
    , "09:10"
    , "10:10"
    , "11:00"
    , "13:30"
    , "14:20"
    , "15:10"
    , "16:20"
    , "17:10"
    , "18:30"
    , "19:20"
    , "20:20"
    , "21:10"
    ]


selectableClassHeader : List String
selectableClassHeader =
    [ "Turma", "Vagas Ocupadas", "Professores", "Salas" ]


selectedCoursesHeaderList : List String
selectedCoursesHeaderList =
    [ "Código", "Turma", "Período" ]


courseToString : Course -> String
courseToString course =
    course.id ++ " - " ++ course.name


classToOccupiedString : Class -> String
classToOccupiedString c =
    "("
        ++ String.fromInt c.enrolled
        ++ (case c.waiting of
                Nothing ->
                    ""

                Just int ->
                    "+" ++ String.fromInt int
           )
        ++ ")/"
        ++ String.fromInt c.capacity


timePlaceListToString : List TimePlace -> String
timePlaceListToString l =
    String.join "\n" (unique (List.map (\timeplace -> timeplace.room) l))
