port module Survey exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (autofocus, class, disabled, id, name, placeholder, selected, type_, value)
import Html.Events exposing (onClick, onFocus, onInput)
import List
import Random.Pcg as Pcg
import Types exposing (..)
import Uuid


type alias Model =
    { title : String
    , description : String
    , tabs : List Tab
    , activeTab : Tab
    , questions : List Question
    , activeQuestionId : Maybe QuestionId
    , uuidSeed : Pcg.Seed
    }


init : ( Model, Cmd Msg )
init =
    let
        ( uuid, seed ) =
            Pcg.step Uuid.uuidGenerator (Pcg.initialSeed 291892861)
    in
        ( { title = ""
          , description = ""
          , tabs = [ "questions", "answers" ]
          , activeTab = "questions"
          , questions = [ newQuestion [] uuid ]
          , activeQuestionId = Nothing
          , uuidSeed = seed
          }
        , Cmd.none
        )



-- UPDATE


type Msg
    = TabClicked Tab
    | TitleEdited String
    | DescriptionEdited String
    | QuestionAdded
    | QuestionClicked QuestionId
    | FormatSelected Question String
    | PromptEdited Question String
    | OptionAdded Question
    | OptionEdited Question Option String
    | OptionRemoved Question Option
    | SelectOptionText Option
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TabClicked tab ->
            { model | activeTab = tab } ! []

        TitleEdited title ->
            { model | title = title } ! []

        DescriptionEdited description ->
            { model | description = description } ! []

        QuestionAdded ->
            let
                ( newUuid, newSeed ) =
                    Pcg.step Uuid.uuidGenerator model.uuidSeed
            in
                { model | uuidSeed = newSeed, questions = model.questions ++ [ newQuestion model.questions newUuid ] } ! []

        QuestionClicked questionId ->
            { model | activeQuestionId = Just questionId } ! []

        FormatSelected question format ->
            { model | questions = List.map (editFormat question format) model.questions } ! []

        PromptEdited question prompt ->
            { model | questions = List.map (editPrompt question prompt) model.questions } ! []

        OptionAdded question ->
            let
                ( newUuid, newSeed ) =
                    Pcg.step Uuid.uuidGenerator model.uuidSeed

                option =
                    newOption question.id newUuid
            in
                { model | uuidSeed = newSeed, questions = List.map (addOption option question) model.questions } ! []

        OptionEdited question option newText ->
            { model | questions = List.map (editOption question option newText) model.questions } ! []

        OptionRemoved question option ->
            { model | questions = List.map (removeOption question option) model.questions } ! []

        SelectOptionText option ->
            model ! [ selectOptionText (Uuid.toString option.id) ]

        NoOp ->
            model ! []


editFormat : Question -> String -> Question -> Question
editFormat editedQuestion newFormat question =
    if question == editedQuestion then
        { question | format = parseQuestionFormat newFormat }
    else
        question


editPrompt : Question -> String -> Question -> Question
editPrompt editedQuestion newPrompt question =
    if question == editedQuestion then
        { question | prompt = newPrompt }
    else
        question


addOption : Option -> Question -> Question -> Question
addOption option addedOnQuestion question =
    if question == addedOnQuestion then
        { question | options = question.options ++ [ option ] }
    else
        question


editOption : Question -> Option -> String -> Question -> Question
editOption editedQuestion editedOption newText question =
    let
        editOptionInQuestion option =
            if option.id == editedOption.id then
                { option | text = newText }
            else
                option
    in
        if question == editedQuestion then
            { question | options = List.map editOptionInQuestion question.options }
        else
            question


removeOption : Question -> Option -> Question -> Question
removeOption removedFromQuestion option question =
    if question == removedFromQuestion then
        { question | options = List.filter ((/=) option) question.options }
    else
        question


port selectOptionText : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "page" ]
        [ div [ class "nav", id "main-menu" ] []
        , div [ class "section" ]
            [ div [ class "container" ]
                [ tabMenu model ]
            , div [ class "container" ]
                [ titleAndDescription model
                , surveySection model
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
        div []
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
            case model.activeQuestionId of
                Just id ->
                    if question.id == id then
                        " raised red"
                    else
                        ""

                Nothing ->
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
    div [ class "field has-addons" ]
        [ div [ class "control" ]
            [ input
                [ type_ "radio"
                , name (toString question.id)
                , onClick NoOp
                ]
                []
            ]
        , div [ class "control is-expanded" ]
            [ input
                [ id (Uuid.toString option.id)
                , class "input is-small is-borderless"
                , value option.text
                , placeholder "Option ..."
                , onInput (OptionEdited question option)
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
