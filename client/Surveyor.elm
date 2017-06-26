port module Surveyor exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onFocus, onInput)
import List
import Types exposing (..)
import Uuid
import WS


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            initialModel flags.startTime
    in
        ( model, WS.register model )



-- UPDATE


type Msg
    = TabClicked Tab
    | TitleEdited String
    | DescriptionEdited String
    | QuestionAdded
    | QuestionClicked (Maybe QuestionId)
    | FormatSelected Question String
    | PromptEdited Question String
    | OptionAdded Question
    | OptionEdited Option String
    | OptionRemoved Question Option
    | SelectOptionText Option
    | ReceiveMessage String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TabClicked tab ->
            { model | activeTab = tab } ! []

        TitleEdited title ->
            WS.send { model | title = title }

        DescriptionEdited description ->
            WS.send { model | description = description }

        QuestionAdded ->
            WS.send (addQuestion model)

        QuestionClicked questionId ->
            { model | activeQuestionId = questionId } ! []

        FormatSelected question format ->
            WS.send (editQuestion model question (editFormat format))

        PromptEdited question prompt ->
            WS.send (editQuestion model question (editPrompt prompt))

        OptionAdded question ->
            WS.send (addOption model question)

        OptionEdited option newText ->
            WS.send (editOption model option (editOptionText newText))

        OptionRemoved question option ->
            WS.send { model | questions = removeOption model.questions option }

        SelectOptionText option ->
            model ! [ selectOptionText option ]

        ReceiveMessage message ->
            WS.receive model message ! []


addQuestion : Model -> Model
addQuestion model =
    let
        ( newModel, uuid ) =
            newUuid model
    in
        { newModel | questions = model.questions ++ [ newQuestion model.questions uuid ] }


addOption : Model -> Question -> Model
addOption model question =
    let
        ( newModel, uuid ) =
            newUuid model

        option =
            newOption uuid
    in
        (editQuestion newModel question (addOptionToQuestion option))


editQuestion : Model -> Question -> (Question -> Question) -> Model
editQuestion model editedQuestion edit =
    let
        applyIfEdited question =
            if question.id == editedQuestion.id then
                edit question
            else
                question
    in
        { model | questions = List.map applyIfEdited model.questions }


editOption : Model -> Option -> (Option -> Option) -> Model
editOption model editedOption edit =
    let
        applyIfEdited option =
            if option.id == editedOption.id then
                edit option
            else
                option

        editOptionInQuestion question =
            { question | options = List.map applyIfEdited question.options }
    in
        { model | questions = (List.map editOptionInQuestion model.questions) }


editFormat : String -> Question -> Question
editFormat newFormat question =
    { question | format = parseQuestionFormat newFormat }


editPrompt : String -> Question -> Question
editPrompt newPrompt question =
    { question | prompt = newPrompt }


addOptionToQuestion : Option -> Question -> Question
addOptionToQuestion option question =
    { question | options = question.options ++ [ option ] }


editOptionText : String -> Option -> Option
editOptionText newText option =
    { option | text = newText }


removeOption : List Question -> Option -> List Question
removeOption questions removedOption =
    let
        removeOptionFromQuestion question =
            { question | options = List.filter ((/=) removedOption) question.options }
    in
        List.map removeOptionFromQuestion questions


selectOptionText : Option -> Cmd msg
selectOptionText option =
    case option.id of
        Just id ->
            selectOptionTextPort (Uuid.toString id)

        Nothing ->
            Cmd.none


port selectOptionTextPort : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    WS.listen model ReceiveMessage



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "page" ]
        [ navBar model
        , div [ class "section", id "server-messages" ]
            ([ text model.userName ]
                ++ (List.map viewServerMessage model.serverMessages)
            )
        , div [ class "section" ]
            [ div [ class "container" ]
                [ tabMenu model ]
            , div [ class "container" ]
                [ titleAndDescription model
                , surveySection model
                ]
            ]
        ]


viewServerMessage : String -> Html Msg
viewServerMessage message =
    div [] [ text message ]


navBar : Model -> Html Msg
navBar model =
    nav [ class "nav has-shadow" ]
        [ div [ class "container" ]
            [ div [ class "nav-left" ]
                [ a [ class "nav-item" ]
                    [ i [ class "fa fa-arrow-left", attribute "aria-hidden" "true" ] [] ]
                , a [ class "nav-item is-tab is-hidden-mobile is-active" ]
                    [ text "Home" ]
                , a [ class "nav-item is-tab is-hidden-mobile" ]
                    [ text "About" ]
                ]
            , span [ class "nav-toggle" ]
                [ span []
                    []
                , span []
                    []
                , span []
                    []
                ]
            , div [ class "nav-right nav-menu" ]
                [ a [ class "nav-item is-tab is-hidden-tablet is-active" ]
                    [ text "Home" ]
                , a [ class "nav-item is-tab is-hidden-tablet" ]
                    [ text "Features" ]
                , a [ class "nav-item is-tab is-hidden-tablet" ]
                    [ text "About" ]
                , a [ class "nav-item is-tab" ]
                    [ i [ class "fa fa-eye fa-2x", attribute "aria-hidden" "true" ] []
                    ]
                ]
            ]
        ]


tabMenu : Model -> Html Msg
tabMenu model =
    div [ class "tabs is-centered is-large" ]
        [ ul []
            (List.map (tabMenuItem model) model.tabs)
        ]


tabMenuItem : Model -> Tab -> Html Msg
tabMenuItem model tab =
    let
        activeClass =
            if tab == model.activeTab then
                "is-active"
            else
                ""
    in
        li
            [ class activeClass ]
            [ a [ onClick (TabClicked tab) ]
                [ text tab ]
            ]


titleAndDescription : Model -> Html Msg
titleAndDescription model =
    div [ class "section" ]
        [ input
            [ class "input is-large"
            , id "title-input"
            , value model.title
            , placeholder "Untitled form"
            , autofocus True
            , onInput TitleEdited
            ]
            []
        , input
            [ class "input is-medium"
            , id "description-input"
            , value model.description
            , placeholder "Form description"
            , onInput DescriptionEdited
            ]
            []
        ]


surveySection : Model -> Html Msg
surveySection model =
    if model.activeTab == "questions" then
        div [ class "section" ]
            ((List.map (viewQuestion model) model.questions)
                ++ [ addQuestionButton model ]
            )
    else
        div [ class "answers" ]
            [ text "No responses yet." ]


viewQuestion : Model -> Question -> Html Msg
viewQuestion model question =
    let
        options =
            case question.format of
                OpenEnded ->
                    [ textarea [ class "textarea" ] [] ]

                MultiChoice ->
                    [ multiChoiceOptions question ]

                _ ->
                    []
    in
        editableQuestion model question options


editableQuestion : Model -> Question -> List (Html Msg) -> Html Msg
editableQuestion model question elements =
    let
        activeClass =
            case ( model.activeQuestionId, question.id ) of
                ( Just id, Just questionId ) ->
                    if questionId == id then
                        "  is-active"
                    else
                        ""

                _ ->
                    ""
    in
        div
            [ class ("box" ++ activeClass)
            , onClick (QuestionClicked question.id)
            ]
            [ div [ class "columns" ]
                [ div [ class "column is-two-thirds" ]
                    ([ questionPrompt question ] ++ elements)
                , div [ class "column is-one-third" ]
                    [ questionFormatSelect question ]
                ]
            ]


questionPrompt : Question -> Html Msg
questionPrompt question =
    input
        [ class "input"
        , value question.prompt
        , onInput (PromptEdited question)
        ]
        []


questionFormatSelect : Question -> Html Msg
questionFormatSelect question =
    div [ class "field" ]
        [ div [ class "control" ]
            [ span [ class "select" ]
                [ select [ name (toString question.id), onInput (FormatSelected question) ]
                    (List.map (questionFormatOption question) questionFormats)
                ]
            ]
        ]


questionFormatOption : Question -> String -> Html Msg
questionFormatOption question format =
    option
        [ value format
        , selected (parseQuestionFormat format == question.format)
        ]
        [ text format ]


multiChoiceOptions : Question -> Html Msg
multiChoiceOptions question =
    div [ class "radio-buttons is-slightly-padded" ]
        ((List.map (viewOption question) question.options) ++ [ addOptionRadio question ])


viewOption : Question -> Option -> Html Msg
viewOption question option =
    let
        uuid =
            case option.id of
                Just id ->
                    Uuid.toString id

                Nothing ->
                    ""
    in
        div [ class "field has-addons" ]
            [ div [ class "control" ]
                [ input
                    [ type_ "radio"
                    , name (toString question.id)
                    , disabled True
                    ]
                    []
                ]
            , div [ class "control is-expanded" ]
                [ input
                    [ id uuid
                    , class "input is-small is-borderless"
                    , value option.text
                    , placeholder "Option ..."
                    , onInput (OptionEdited option)
                    , onFocus (SelectOptionText option)
                    ]
                    []
                ]
            , div [ class "control" ]
                [ a
                    [ class "button is-danger is-small"
                    , onClick (OptionRemoved question option)
                    ]
                    [ text "x" ]
                ]
            ]


addOptionRadio : Question -> Html Msg
addOptionRadio question =
    div [ class "field has-addons" ]
        [ div [ class "control" ]
            [ input
                [ type_ "radio"
                , name "Add option"
                , onClick (OptionAdded question)
                , disabled True
                ]
                []
            ]
        , div [ class "control is-expanded" ]
            [ input
                [ class "input is-small is-borderless"
                , placeholder "Add option"
                , onFocus (OptionAdded question)
                ]
                []
            ]
        ]


addQuestionButton : Model -> Html Msg
addQuestionButton model =
    div
        [ class "ui bottom attached button is-primary"
        , onClick QuestionAdded
        ]
        [ text "Add Question" ]
