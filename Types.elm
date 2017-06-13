module Types exposing (..)

import Dict exposing (Dict)
import Uuid exposing (Uuid)


type alias Tab =
    String


type alias QuestionId =
    Uuid


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


type alias Option =
    { id : Uuid
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
        , options = []
        }


newOption : QuestionId -> Uuid -> Option
newOption questionId uuid =
    { id = uuid
    , text = ""
    }


questionFormats : List String
questionFormats =
    [ "Open ended", "Multi choice" ]


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
