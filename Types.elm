module Types exposing (..)

import Dict exposing (Dict)


type Question
    = OpenEnded { prompt : String, answer : String }
    | MultiChoice { prompt : String, options : List String, answer : String }
    | NumberRange { prompt : String, range : ( Int, Int ), answer : Int }
    | OrdinalScale { prompt : String, options : List String, answers : Dict Int String }
