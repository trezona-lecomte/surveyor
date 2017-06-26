module JSON exposing (encodeModel, decodeSurvey)

import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Types exposing (..)
import Uuid exposing (Uuid)


decodeSurvey : String -> Result String Survey
decodeSurvey =
    decodeString surveyDecoder


surveyDecoder : Decoder Survey
surveyDecoder =
    map3 Survey
        (field "title" Decode.string)
        (field "description" Decode.string)
        (field "questions" (Decode.list questionDecoder))


questionDecoder : Decoder Question
questionDecoder =
    map4 Question
        (field "questionId" uuidDecoder)
        (field "format" questionFormatDecoder)
        (field "prompt" Decode.string)
        (field "options" (Decode.list optionDecoder))


optionDecoder : Decoder Option
optionDecoder =
    map2 Option
        (field "optionId" uuidDecoder)
        (field "text" Decode.string)


uuidDecoder : Decoder (Maybe Uuid)
uuidDecoder =
    Decode.string |> andThen uuidFromString


questionFormatDecoder : Decoder QuestionFormat
questionFormatDecoder =
    Decode.string |> andThen questionFormatFromString


uuidFromString : String -> Decoder (Maybe Uuid)
uuidFromString uuid =
    case Uuid.fromString uuid of
        Just id ->
            Decode.succeed (Just id)

        Nothing ->
            Decode.fail uuid


questionFormatFromString : String -> Decoder QuestionFormat
questionFormatFromString format =
    case format of
        "OpenEnded" ->
            Decode.succeed OpenEnded

        "MultiChoice" ->
            Decode.succeed MultiChoice

        "NumberRange" ->
            Decode.succeed NumberRange

        "OrdinalScale" ->
            Decode.succeed OrdinalScale

        _ ->
            Decode.fail format


modelEncoder : Model -> Decode.Value
modelEncoder model =
    Encode.object
        [ ( "title", Encode.string model.title )
        , ( "description", Encode.string model.description )
        , ( "questions", Encode.list (List.map encodeQuestion model.questions) )
        ]


encodeModel : Model -> String
encodeModel model =
    encode 0 (modelEncoder model)


encodeQuestion : Question -> Encode.Value
encodeQuestion question =
    let
        questionIdEncoder =
            case question.id of
                Just id ->
                    Encode.string (Uuid.toString id)

                Nothing ->
                    Encode.string ""
    in
        Encode.object
            [ ( "questionId", questionIdEncoder )
            , ( "format", Encode.string (toString question.format) )
            , ( "prompt", Encode.string question.prompt )
            , ( "options", Encode.list (List.map encodeOption question.options) )
            ]


encodeOption : Option -> Encode.Value
encodeOption option =
    let
        encodedUuid =
            case option.id of
                Just id ->
                    Encode.string (Uuid.toString id)

                Nothing ->
                    Encode.string ""
    in
        Encode.object
            [ ( "optionId", encodedUuid )
            , ( "text", Encode.string option.text )
            ]
