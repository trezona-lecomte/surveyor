module SurveyJson exposing (encodeModel, decodeSurvey)

import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Types exposing (..)
import Uuid


-- decodeString : Decoder a -> String -> Result String a
-- list : Decoder a -> Decoder (List a)
-- field : String -> Decoder a -> Decoder a


decodeSurvey : String -> Result String Survey
decodeSurvey =
    decodeString surveyDecoder


surveyDecoder =
    map3 Survey
        (field "title" Decode.string)
        (field "description" Decode.string)
        (field "questions" (Decode.list questionDecoder))


questionDecoder =
    map4 Question
        (field "id" uuidDecoder)
        (field "format" questionFormatDecoder)
        (field "prompt" Decode.string)
        (field "options" (Decode.list optionDecoder))


optionDecoder =
    map2 Option
        (field "id" uuidDecoder)
        (field "text" Decode.string)


uuidDecoder =
    Decode.string |> andThen uuidFromString


questionFormatDecoder =
    Decode.string |> andThen questionFormatFromString


uuidFromString uuid =
    case Uuid.fromString uuid of
        Just id ->
            Decode.succeed (Just id)

        Nothing ->
            Decode.fail uuid


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



-- Decoder (Maybe Types.QuestionId)
-- -> Decoder Types.QuestionFormat
-- -> Decoder String
-- -> Decoder (List Option -> Question)
-- pointDecoder = Json.Decode.map2 Point (Json.Decode.field "x" Json.Decode.int) (Json.Decode.field "y" Json.Decode.int)


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
            [ ( "id", questionIdEncoder )
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
            [ ( "id", encodedUuid )
            , ( "text", Encode.string option.text )
            ]
