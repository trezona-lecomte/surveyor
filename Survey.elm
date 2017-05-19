module Survey exposing (Model, Msg, init, update, view, subscriptions)

import Dict exposing (Dict)
import Html exposing (Html, a, div, fieldset, input, label, option, select, text, textarea)
import Html.Attributes exposing (autofocus, class, id, name, placeholder, type_, value)
import Html.Events exposing (onClick)
import List exposing (map)
import SampleData exposing (questions)
import Types exposing (..)


type alias Model =
    { title : String
    , description : String
    , questions : List Question
    , tabs : List Tab
    , activeTab : Tab
    }


type alias Tab =
    String


init : ( Model, Cmd Msg )
init =
    Model "Untitled form" "" SampleData.questions [ "questions", "answers" ] "questions" ! []



-- UPDATE


type Msg
    = ActivateTab Tab
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ActivateTab tab ->
            { model | activeTab = tab } ! []

        NoOp ->
            model ! []



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "page" ]
        [ div [ class "ui secondary pointing menu", id "main-menu" ] []
        , div [ class "ui content container" ]
            [ tabMenu model
            , div [ class "ui segments" ]
                [ titleAndDescription model
                , surveySection model
                ]
            ]
        ]


tabMenu : Model -> Html Msg
tabMenu model =
    div [ class "ui large secondary pointing menu", id "tab-menu" ]
        (map (tabMenuItem model) model.tabs)


tabMenuItem : Model -> Tab -> Html Msg
tabMenuItem model tab =
    let
        tabClass =
            if tab == model.activeTab then
                "active item"
            else
                "item"
    in
        a [ class tabClass, onClick (ActivateTab tab) ] [ text tab ]


titleAndDescription : Model -> Html Msg
titleAndDescription model =
    div [ class "ui segment", id "title-and-description" ]
        [ div [ class "ui massive fluid xtransparent input" ]
            [ input [ type_ "text", value model.title, autofocus True ] [] ]
        , div [ class "ui large fluid xtransparent input" ]
            [ input [ type_ "text", value model.description, placeholder "Form description" ] [] ]
        ]


surveySection : Model -> Html Msg
surveySection model =
    if model.activeTab == "questions" then
        div [ class "questions" ]
            (map viewQuestion model.questions)
    else
        div [ class "answers" ]
            [ text "No responses yet." ]


viewQuestion : Question -> Html Msg
viewQuestion question =
    case question of
        OpenEnded q ->
            openEndedQuestion q

        MultiChoice q ->
            multiChoiceQuestion q

        NumberRange q ->
            numberRangeQuestion q

        OrdinalScale q ->
            ordinalScaleQuestion q


openEndedQuestion : { prompt : String, answer : String } -> Html Msg
openEndedQuestion { prompt, answer } =
    div [ class "ui segment question" ]
        [ (text prompt)
        , textarea [ value answer ] []
        ]


multiChoiceQuestion : { prompt : String, options : List String, answer : String } -> Html Msg
multiChoiceQuestion { prompt, options, answer } =
    div [ class "ui segment question" ]
        [ text prompt
        , fieldset [ class "radio-buttons" ] (map (radio NoOp prompt) options)
        ]


numberRangeQuestion : { prompt : String, range : ( Int, Int ), answer : Int } -> Html Msg
numberRangeQuestion { prompt, range, answer } =
    div [ class "ui segment question" ]
        [ (text prompt)
        , select [] (map optionForNumber (List.range (Tuple.first range) (Tuple.second range)))
        ]


optionForNumber : Int -> Html Msg
optionForNumber number =
    option [ value (toString number) ] [ text (toString number) ]


ordinalScaleQuestion : { prompt : String, options : List String, answers : Dict Int String } -> Html Msg
ordinalScaleQuestion { prompt, options, answers } =
    div []
        [ (text prompt)
        , fieldset [] (map (radio NoOp prompt) options)
        ]


radio : msg -> String -> String -> Html msg
radio msg prompt option =
    label []
        [ input [ type_ "radio", name prompt, onClick msg ] []
        , text option
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
