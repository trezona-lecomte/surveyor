module Main exposing (..)

import Html exposing (program)
import Surveyor exposing (Msg, init, view, update, subscriptions)
import Types exposing (Model)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
