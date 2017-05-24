module Types exposing (..)

import Dict exposing (Dict)
import Uuid exposing (Uuid)


type alias Question =
    { id : Uuid
    , format : QuestionFormat
    , prompt : String
    , options : List Option
    , active : Bool
    }


type QuestionFormat
    = OpenEnded
    | MultiChoice
    | NumberRange
    | OrdinalScale


type alias Option =
    { index : Int
    , text : String
    }


type Answer
    = OpenAnswer String
    | MultiAnswer String
    | NumberAnswer Int
    | OrdinalAnswer (Dict Int String)


newQuestion : List Question -> Uuid -> Question
newQuestion existingQuestions uuid =
    let
        newQuestionNumber =
            List.length existingQuestions + 1
    in
        { id = uuid
        , format = MultiChoice
        , prompt = "Untitled Question " ++ (toString newQuestionNumber)
        , options = [ newOption [] ]
        , active = False
        }


newOption : List Option -> Option
newOption existingOptions =
    let
        newIndex =
            List.length existingOptions
    in
        { index = newIndex, text = "Option " ++ (toString (newIndex + 1)) }
