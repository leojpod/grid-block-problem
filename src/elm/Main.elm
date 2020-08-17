module Main exposing (main)

import Browser
import Html exposing (Html, div, span, table, td, text, tr)
import Html.Attributes exposing (class)
import Html.Attributes.Extra


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Cell
    = Empty
    | Fill


type alias Board =
    List (List Cell)


type alias Point =
    ( Int, Int )


type alias Group =
    List Point


type alias Model =
    { board : Board
    , groups : List Group
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { board = []
      , groups = []
      }
    , Cmd.none
    )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Document Title"
    , body =
        [ div [ class "flex flex-col items-center justify-center min-h-screen text-6xl" ]
            [ boardView model
            ]
        ]
    }


boardView : Model -> Html Msg
boardView { board, groups } =
    table [ class "table-fixed" ] <|
        List.map (boardLineView groups) board


boardLineView : List Group -> List Cell -> Html Msg
boardLineView groups line =
    tr [] <| List.map (\cell -> td [ Html.Attributes.Extra.attributeIf (cell == Fill) <| class "bg-red" ] []) line
