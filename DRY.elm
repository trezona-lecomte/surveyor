module DRY exposing (..)


noCmd : model -> ( model, Cmd msg )
noCmd model =
    model ! []
