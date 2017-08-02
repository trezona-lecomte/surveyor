module WS exposing (register, listen, send, receive)

import JSON
import Types exposing (..)
import WebSocket


server : String
server =
    "ws://0.0.0.0:8000"


register : Model -> Cmd msg
register model =
    -- TODO: Use JSON-encoded 'action' for registering.
    WebSocket.send server ("register as " ++ model.userName)


listen : Model -> (String -> msg) -> Sub msg
listen model =
    WebSocket.listen server


send : Model -> ( Model, Cmd msg )
send model =
    model ! [ WebSocket.send server (JSON.encodeModel model) ]


receive : Model -> String -> Model
receive model message =
    case JSON.decodeSurvey message of
        Ok newSurvey ->
            { model
                | title = newSurvey.title
                , description = newSurvey.description
                , questions = newSurvey.questions
                , serverMessages = message :: model.serverMessages
            }

        Err error ->
            { model | serverMessages = [ error, message ] ++ model.serverMessages }
