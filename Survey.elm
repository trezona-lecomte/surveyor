module Survey exposing (Model, Msg, init, update, view, subscriptions)

-- import Dict exposing (Dict)

import Html exposing (Html, a, div, fieldset, input, label, option, select, text, textarea)
import Html.Attributes exposing (autofocus, class, id, name, placeholder, type_, value)
import Html.Events exposing (onClick, onFocus, onInput)
import List
import Set
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
    Model "Untitled form" "" [ defaultQuestion 1 ] [ "questions", "answers" ] "questions" ! []


defaultQuestion : Int -> Question
defaultQuestion number =
    { format = MultiChoice
    , prompt = "Untitled Question " ++ (toString number)
    , options = [ "Option 1" ]
    , active = False
    }



-- UPDATE


type Msg
    = TabClicked Tab
    | QuestionClicked String
    | TitleEdited String
    | DescriptionEdited String
    | PromptEdited Question String
    | OptionAdded Question
    | OptionEdited Question Option Option
    | QuestionAdded
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TabClicked tab ->
            { model | activeTab = tab } ! []

        QuestionClicked prompt ->
            { model | questions = List.map (toggleEditingQuestion prompt) model.questions } ! []

        TitleEdited title ->
            { model | title = title } ! []

        DescriptionEdited description ->
            { model | description = description } ! []

        PromptEdited question prompt ->
            { model | questions = List.map (editPrompt question prompt) model.questions } ! []

        OptionAdded question ->
            { model | questions = List.map (addOption question) model.questions } ! []

        OptionEdited question option newOption ->
            { model | questions = List.map (editOptionInQuestion question option newOption) model.questions } ! []

        QuestionAdded ->
            { model | questions = model.questions ++ [ defaultQuestion (List.length model.questions + 1) ] } ! []

        NoOp ->
            model ! []


toggleEditingQuestion : String -> Question -> Question
toggleEditingQuestion prompt question =
    let
        alreadyActive =
            question.active

        newlyActive =
            question.prompt == prompt
    in
        { question | active = alreadyActive || newlyActive }


editPrompt : Question -> String -> Question -> Question
editPrompt editedQuestion newPrompt question =
    if question == editedQuestion then
        { question | prompt = newPrompt }
    else
        question


editOptionInQuestion : Question -> Option -> Option -> Question -> Question
editOptionInQuestion editedQuestion oldOption newOption question =
    let
        editOption o =
            if o == oldOption then
                newOption
            else
                o
    in
        if question == editedQuestion then
            { question | options = List.map editOption question.options }
        else
            question


addOption : Question -> Question -> Question
addOption addedOnQuestion question =
    let
        additionalOption =
            "Option " ++ (toString (1 + List.length question.options))
    in
        if question == addedOnQuestion then
            { question | options = question.options ++ [ additionalOption ] }
        else
            question



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
        (List.map (tabMenuItem model) model.tabs)


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
        [ div [ class "ui massive fluid input" ]
            [ input [ type_ "text", value model.title, autofocus True, onInput TitleEdited ] [] ]
        , div [ class "ui large fluid input" ]
            [ input [ type_ "text", value model.description, placeholder "Form description", onInput DescriptionEdited ] [] ]
        ]


surveySection : Model -> Html Msg
surveySection model =
    if model.activeTab == "questions" then
        div [ class "ui form questions" ]
            [ div [ class "grouped fields" ]
                ((List.map (viewQuestion model) model.questions)
                    ++ [ addQuestionButton model ]
                )
            ]
    else
        div [ class "answers" ]
            [ text "No responses yet." ]


addQuestionButton : Model -> Html Msg
addQuestionButton model =
    div [ class "ui bottom attached button", onClick QuestionAdded ]
        [ text "Add Question" ]


viewQuestion : Model -> Question -> Html Msg
viewQuestion model question =
    let
        options =
            case question.format of
                MultiChoice ->
                    [ multiChoiceOptions question ]

                _ ->
                    []
    in
        editableQuestion question options


multiChoiceOptions : Question -> Html Msg
multiChoiceOptions question =
    fieldset [ class "radio-buttons" ]
        ((List.map (radio question) question.options) ++ [ addOptionRadio question ])


radio : Question -> String -> Html Msg
radio question option =
    div [ class "field" ]
        [ div [ class "ui radio checkbox" ]
            [ input [ type_ "radio", name question.prompt, onClick NoOp ] []
            , label []
                [ div [ class "ui transparent input" ]
                    [ input [ value option, onInput (OptionEdited question option) ] []
                    ]
                ]
            ]
        ]


addOptionRadio : Question -> Html Msg
addOptionRadio question =
    div [ class "field" ]
        [ div [ class "ui radio checkbox" ]
            [ input [ type_ "radio", name "Add option", onClick (OptionAdded question) ] []
            , label []
                [ div [ class "ui transparent input" ]
                    [ input [ placeholder "Add option", onFocus (OptionAdded question) ] []
                    ]
                ]
            ]
        ]


editableQuestion : Question -> List (Html Msg) -> Html Msg
editableQuestion question elements =
    let
        activeClass =
            if question.active then
                " raised red"
            else
                ""
    in
        div
            [ class ("ui segment question" ++ activeClass)
            , onClick (QuestionClicked question.prompt)
            ]
            ([ questionPrompt question ] ++ elements)


questionPrompt : Question -> Html Msg
questionPrompt question =
    input
        [ type_ "text"
        , value question.prompt
        , onInput (PromptEdited question)
        ]
        []


optionForNumber : Int -> Html Msg
optionForNumber number =
    option [ value (toString number) ] [ text (toString number) ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
