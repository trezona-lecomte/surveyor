module Main exposing (..)

import Html exposing (program)
import Surveyor exposing (Msg, init, view, update, subscriptions)
import Types exposing (Flags, Model)


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
