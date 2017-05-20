module Survey exposing (Model, Msg, init, update, view, subscriptions)

-- import Dict exposing (Dict)

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
    = TabClicked Tab
    | QuestionClicked String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TabClicked tab ->
            { model | activeTab = tab } ! []

        QuestionClicked prompt ->
            { model | questions = List.map (toggleEditingQuestion prompt) model.questions } ! []

        NoOp ->
            model ! []


toggleEditingQuestion : String -> Question -> Question
toggleEditingQuestion prompt question =
    let
        newEditing q =
            if (Debug.log "question prompt" q.prompt) == (Debug.log "clicked prompt" prompt) then
                (not q.editing)
            else if q.editing then
                (not q.editing)
            else
                q.editing
    in
        case question of
            OpenEnded q ->
                OpenEnded
                    { q | editing = newEditing q }

            MultiChoice q ->
                MultiChoice { q | editing = newEditing q }

            NumberRange q ->
                NumberRange { q | editing = newEditing q }

            OrdinalScale q ->
                OrdinalScale { q | editing = newEditing q }



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
        a [ class tabClass, onClick (TabClicked tab) ] [ text tab ]


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
        div [ class "ui form questions" ]
            [ div [ class "grouped fields" ]
                (map (viewQuestion model) model.questions)
            ]
    else
        div [ class "answers" ]
            [ text "No responses yet." ]


viewQuestion : Model -> Question -> Html Msg
viewQuestion model question =
    case question of
        OpenEnded q ->
            (editableQuestion model
                q
                [ textarea [ value q.answer ] []
                ]
            )

        MultiChoice q ->
            (editableQuestion model
                q
                [ fieldset [ class "radio-buttons" ] (map (radio NoOp q.prompt) q.options)
                ]
            )

        NumberRange q ->
            (editableQuestion model
                q
                [ select [] (map optionForNumber (List.range (Tuple.first q.range) (Tuple.second q.range)))
                ]
            )

        OrdinalScale q ->
            (editableQuestion model
                q
                [ fieldset [] (map (radio NoOp q.prompt) q.options)
                ]
            )


editableQuestion : Model -> { a | prompt : String, editing : Bool } -> List (Html Msg) -> Html Msg
editableQuestion model { prompt, editing } elements =
    let
        editingClass =
            if editing then
                " raised red"
            else
                ""
    in
        div
            [ class ("ui segment question" ++ editingClass)
            , onClick (QuestionClicked prompt)
            ]
            ([ text prompt ] ++ elements)


optionForNumber : Int -> Html Msg
optionForNumber number =
    option [ value (toString number) ] [ text (toString number) ]


radio : msg -> String -> String -> Html msg
radio msg prompt option =
    div [ class "field" ]
        [ div [ class "ui radio checkbox" ]
            [ input [ type_ "radio", name prompt, onClick msg ] []
            , label []
                [ text option
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
