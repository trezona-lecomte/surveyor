module Survey exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (Html, a, div, fieldset, h1, h2, h3, h4, h5, h6, i, input, label, option, select, text, textarea)
import Html.Attributes exposing (autofocus, class, id, name, placeholder, type_, value)
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


type alias Tab =
    String


init : ( Model, Cmd Msg )
init =
    let
        ( uuid, seed ) =
            Pcg.step Uuid.uuidGenerator (Pcg.initialSeed 291892861)
    in
        { title = ""
        , description = ""
        , tabs = [ "questions", "answers" ]
        , activeTab = "questions"
        , questions = [ newQuestion [] uuid ]
        , activeQuestionId = Nothing
        , uuidSeed = seed
        }
            ! []



-- UPDATE


type Msg
    = TabClicked Tab
    | QuestionClicked QuestionId
    | TitleEdited String
    | DescriptionEdited String
    | PromptEdited Question String
    | OptionAdded Question
    | OptionEdited Question Option String
    | QuestionAdded
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TabClicked tab ->
            { model | activeTab = tab } ! []

        QuestionClicked questionId ->
            { model | activeQuestionId = Just questionId } ! []

        TitleEdited title ->
            { model | title = title } ! []

        DescriptionEdited description ->
            { model | description = description } ! []

        PromptEdited question prompt ->
            { model | questions = List.map (editPrompt question prompt) model.questions } ! []

        OptionAdded question ->
            { model | questions = List.map (addOption question) model.questions } ! []

        OptionEdited question option newText ->
            { model | questions = List.map (editOptionInQuestion question option newText) model.questions } ! []

        QuestionAdded ->
            let
                ( newUuid, newSeed ) =
                    Pcg.step Uuid.uuidGenerator model.uuidSeed
            in
                { model | uuidSeed = newSeed, questions = model.questions ++ [ newQuestion model.questions newUuid ] } ! []

        NoOp ->
            model ! []


editPrompt : Question -> String -> Question -> Question
editPrompt editedQuestion newPrompt question =
    if question == editedQuestion then
        { question | prompt = newPrompt }
    else
        question


editOptionInQuestion : Question -> Option -> String -> Question -> Question
editOptionInQuestion editedQuestion editedOption newText question =
    let
        editOption option =
            if option.index == editedOption.index then
                { option | text = newText }
            else
                option
    in
        if question == editedQuestion then
            { question | options = List.map editOption question.options }
        else
            question


addOption : Question -> Question -> Question
addOption addedOnQuestion question =
    if question == addedOnQuestion then
        { question | options = question.options ++ [ newOption question.options ] }
    else
        question



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "page" ]
        [ div [ class "ui secondary pointing menu", id "main-menu" ] []
        , div [ class "ui content container", id "main-container" ]
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
        a [ class tabClass, onClick (TabClicked tab) ] [ h3 [] [ text tab ] ]


titleAndDescription : Model -> Html Msg
titleAndDescription model =
    div [ class "ui segment" ]
        [ div [ class "ui massive fluid input" ]
            [ h1 [] [ input [ id "title-input", value model.title, placeholder "Untitled form", autofocus True, onInput TitleEdited ] [] ] ]
        , div [ class "ui large fluid input" ]
            [ h2 [] [ input [ id "description-input", value model.description, placeholder "Form description", onInput DescriptionEdited ] [] ] ]
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
            [ class ("ui segment question" ++ activeClass)
            , onClick (QuestionClicked question.id)
            ]
            [ div [ class "ui two column grid" ]
                [ div [ class "twelve wide column" ]
                    ([ questionPrompt question ] ++ elements)
                , div [ class "four wide column" ]
                    [ questionFormatSelect question ]
                ]
            ]


questionFormatSelect : Question -> Html Msg
questionFormatSelect question =
    div [ class "ui compact menu" ]
        [ div [ class "ui simple dropdown item" ]
            [ text (toString question.format)
            , i [ class "dropdown icon" ] []
            , (div [ class "menu" ]
                (List.map questionFormatOption questionFormats)
              )
            ]
        ]


questionFormatOption : String -> Html Msg
questionFormatOption format =
    div [ class "item" ]
        [ i [ class "folder icon" ]
            []
        , (text format)
        ]


questionPrompt : Question -> Html Msg
questionPrompt question =
    h3 []
        [ input
            [ value question.prompt
            , onInput (PromptEdited question)
            ]
            []
        ]


multiChoiceOptions : Question -> Html Msg
multiChoiceOptions question =
    fieldset [ class "radio-buttons" ]
        ((List.map (optionRadio question) question.options) ++ [ addOptionRadio question ])


optionRadio : Question -> Option -> Html Msg
optionRadio question option =
    div [ class "field" ]
        [ div [ class "ui radio checkbox" ]
            [ input [ type_ "radio", name question.prompt, onClick NoOp ] []
            , label []
                [ div [ class "ui transparent input" ]
                    [ h5 [] [ input [ value option.text, onInput (OptionEdited question option) ] [] ]
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


addQuestionButton : Model -> Html Msg
addQuestionButton model =
    div [ class "ui bottom attached button", onClick QuestionAdded ]
        [ text "Add Question" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
