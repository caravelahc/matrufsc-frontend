module Utils exposing (courseToString, schoolDays, timeSlots)

import Api exposing (Course)


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


courseToString : Course -> String
courseToString course =
    course.id ++ " - " ++ course.name
