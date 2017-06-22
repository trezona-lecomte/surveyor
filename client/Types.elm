module Types exposing (..)

import Dict exposing (Dict)
import Random.Pcg as Pcg
import Uuid exposing (Uuid)


type alias Model =
    { userName : String
    , title : String
    , description : String
    , tabs : List Tab
    , activeTab : Tab
    , questions : List Question
    , activeQuestionId : Maybe QuestionId
    , uuidSeed : Pcg.Seed
    , serverSocketAddress : String
    , serverMessages : List String
    }


type alias Flags =
    { startTime : Int }


type alias Survey =
    { title : String
    , description : String
    , questions : List Question
    }


type alias Question =
    { id : Maybe QuestionId
    , format : QuestionFormat
    , prompt : String
    , options : List Option
    }


type alias Option =
    { id : Maybe OptionId
    , text : String
    }


type QuestionFormat
    = OpenEnded
    | MultiChoice
    | NumberRange
    | OrdinalScale


type Answer
    = OpenAnswer String
    | MultiAnswer String
    | NumberAnswer Int
    | OrdinalAnswer (Dict Int String)


type alias QuestionId =
    Uuid


type alias OptionId =
    Uuid


type alias Tab =
    String


initialModel : Int -> Model
initialModel startTime =
    let
        ( uuid, seed ) =
            Pcg.step Uuid.uuidGenerator (Pcg.initialSeed startTime)
    in
        { userName = "foo"
        , title = ""
        , description = ""
        , tabs = [ "questions", "answers" ]
        , activeTab = "questions"
        , questions = [ newQuestion [] uuid ]
        , activeQuestionId = Nothing
        , uuidSeed = seed
        , serverSocketAddress = "0.0.0.0:8000"
        , serverMessages = []
        }


newQuestion : List Question -> Uuid -> Question
newQuestion existingQuestions uuid =
    let
        newQuestionNumber =
            List.length existingQuestions + 1
    in
        { id = Just uuid
        , format = MultiChoice
        , prompt = "Untitled Question " ++ (toString newQuestionNumber)
        , options = []
        }


newOption : Maybe QuestionId -> Uuid -> Maybe Option
newOption questionId uuid =
    case questionId of
        Just id ->
            Just
                { id = Just uuid
                , text = ""
                }

        Nothing ->
            Nothing


questionFormats : List String
questionFormats =
    [ "Multi choice", "Open ended" ]


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
