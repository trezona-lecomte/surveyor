module Types exposing (..)

import Dict exposing (Dict)


type Question
    = OpenEnded { editing : Bool, prompt : String, answer : String }
    | MultiChoice { editing : Bool, prompt : String, options : List String, answer : String }
    | NumberRange { editing : Bool, prompt : String, range : ( Int, Int ), answer : Int }
    | OrdinalScale { editing : Bool, prompt : String, options : List String, answers : Dict Int String }
