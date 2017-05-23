module Survey exposing (Model, Msg, init, update, view, subscriptions)

-- import Dict exposing (Dict)

import Html exposing (Html, a, div, fieldset, input, label, option, select, text, textarea)
import Html.Attributes exposing (autofocus, class, id, name, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import List exposing (map)
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
    Model "Untitled form" "" defaultQuestions [ "questions", "answers" ] "questions" ! []


defaultQuestions : List Question
defaultQuestions =
    [ { format = MultiChoice
      , prompt = "Untitled Question"
      , options = [ "Option 1" ]
      , active = False
      }
    ]



-- UPDATE


type Msg
    = TabClicked Tab
    | QuestionClicked String
    | TitleEdited String
    | DescriptionEdited String
      -- | PromptEdited Question String
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

        -- PromptEdited ( question, prompt ) ->
        --     { model | questions = description == description } ! []
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
                (map (viewQuestion model) model.questions)
            ]
    else
        div [ class "answers" ]
            [ text "No responses yet." ]


viewQuestion : Model -> Question -> Html Msg
viewQuestion model question =
    case question.format of
        OpenEnded ->
            (editableQuestion question
                []
            )

        MultiChoice ->
            (editableQuestion question
                [ fieldset [ class "radio-buttons" ] (map (radio NoOp question.prompt) question.options)
                ]
            )

        NumberRange ->
            (editableQuestion question
                [ select [] []
                ]
            )

        OrdinalScale ->
            (editableQuestion question
                [ fieldset [] (map (radio NoOp question.prompt) question.options)
                ]
            )


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
questionPrompt { prompt } =
    input
        [ type_ "text"
        , value prompt
          -- , onInput PromptEdited questiony
        ]
        []


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
