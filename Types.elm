module Types exposing (..)

import Dict exposing (Dict)
import Uuid exposing (Uuid)


type alias Tab =
    String


type QuestionId
    = QuestionId Uuid


type alias Question =
    { id : QuestionId
    , format : QuestionFormat
    , prompt : String
    , options : List Option
    }


type QuestionFormat
    = OpenEnded
    | MultiChoice
    | NumberRange
    | OrdinalScale


questionFormats : List String
questionFormats =
    [ "Open ended", "Multi choice" ]


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
        { id = QuestionId uuid
        , format = MultiChoice
        , prompt = "Untitled Question " ++ (toString newQuestionNumber)
        , options = [ newOption [] ]
        }


newOption : List Option -> Option
newOption existingOptions =
    let
        newIndex =
            List.length existingOptions
    in
        { index = newIndex, text = "Option " ++ (toString (newIndex + 1)) }


parseQuestionFormat : String -> QuestionFormat
parseQuestionFormat format =
    case format of
        "Open ended" ->
            OpenEnded

        "Multi choice" ->
            MultiChoice

        "Number range" ->
            NumberRange

        "Ordinal scale" ->
            OrdinalScale

        _ ->
            OpenEnded
