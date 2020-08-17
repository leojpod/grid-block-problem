module Main exposing (Board, Cell(..), Group, computeGroups, main)

import Browser
import Html exposing (Html, div, table, td, text, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Extra
import List
import List.Extra
import Random


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


boardGenerator : Int -> Random.Generator Board
boardGenerator size =
    Random.list size <|
        Random.list size <|
            Random.uniform Empty [ Fill ]


generateBoard : Int -> Cmd Msg
generateBoard =
    Random.generate NewBoard << boardGenerator


type alias Point =
    ( Int, Int )


type alias Group =
    List Point


isConnectedGroup : Int -> Int -> Group -> Bool
isConnectedGroup x y =
    List.any
        (\( x_, y_ ) ->
            (x == x_ && (y_ == y + 1 || y_ == y - 1))
                || (y == y_ && (x_ == x + 1 || x_ == x - 1))
        )


partitionGroups : Int -> Int -> List Group -> ( List Group, List Group )
partitionGroups x y =
    List.foldl
        (\group ( connected, notConnected ) ->
            if isConnectedGroup x y group then
                ( group :: connected, notConnected )

            else
                ( connected, group :: notConnected )
        )
        ( [], [] )


computeGroups : Board -> List Group
computeGroups board =
    --| step 1: from matrix to list of things
    board
        |> List.indexedMap
            (\x ->
                List.indexedMap
                    (\y cell ->
                        if cell == Fill then
                            Just ( x, y )

                        else
                            Nothing
                    )
            )
        |> List.concatMap identity
        |> List.filterMap identity
        --| step 2 reduce that list according to the following
        --| for each filled up cell, find all the groups that connect to it:
        --| if there are none, create one with the cell in items--| if there is just one, add the cell to it
        --| if there are more than one then merge them
        |> List.foldl
            (\( x, y ) groups ->
                partitionGroups x y groups
                    |> (\( connectedGroups, notConnectedGroups ) ->
                            case connectedGroups of
                                [] ->
                                    [ ( x, y ) ] :: groups

                                _ ->
                                    (( x, y ) :: List.concat connectedGroups) :: notConnectedGroups
                       )
            )
            []
        --| NOTE: the sorting is mostly just so that the test pass nicely
        |> List.map List.sort
        |> List.sort


type alias State =
    { board : Board
    , groups : List Group
    }


type alias UIState =
    { selected : Maybe Point
    , hovered : Maybe Group
    }


type alias Model =
    { state :
        State
    , uiState :
        UIState
    }


type alias Flags =
    ()



{-
   init zone
-}


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { state =
            { board = []
            , groups = []
            }
      , uiState = { selected = Nothing, hovered = Nothing }
      }
    , generateBoard 10
    )



{-
   update and subscriptions area
-}


type Msg
    = NewBoard Board
    | SelectCell Point
    | HoverGroupFor Point
    | ResetHover


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ state, uiState } as model) =
    case msg of
        NewBoard newBoard ->
            ( { model
                | state =
                    { state
                        | board = newBoard
                        , groups = computeGroups newBoard
                    }
                , uiState = { uiState | selected = Nothing, hovered = Nothing }
              }
            , Cmd.none
            )

        SelectCell selected ->
            ( { model
                | uiState =
                    { uiState
                        | selected = Just selected
                    }
              }
            , Cmd.none
            )

        HoverGroupFor point ->
            List.Extra.find (List.member point) state.groups
                --| Note: Usually I'd use a lot of Maybe.Extra.unwrap instead of sequences of Maybe.map followed by Maybe.withDefault
                |> Maybe.map
                    (\group ->
                        ( { model | uiState = { uiState | hovered = Just group } }
                        , Cmd.none
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        ResetHover ->
            ( { model | uiState = { uiState | hovered = Nothing } }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



{-
   view and stuff
-}


view : Model -> Browser.Document Msg
view model =
    { title = "Quick Elm Demo"
    , body =
        [ div [ class "flex flex-col items-center justify-center min-h-screen text-xl" ]
            [ boardView model
            ]
        ]
    }


boardView : Model -> Html Msg
boardView { state, uiState } =
    table [ class "table-fixed", onClick ResetHover ] <|
        List.indexedMap (boardLineView state.groups uiState) state.board


boardLineView : List Group -> UIState -> Int -> List Cell -> Html Msg
boardLineView groups { selected, hovered } x line =
    tr [] <|
        List.indexedMap
            (\y cell ->
                td
                    (class "w-12 h-12 text-center align-middle "
                        :: (if cell == Fill then
                                let
                                    isHovered =
                                        Maybe.map (List.member ( x, y )) hovered
                                            |> Maybe.withDefault False
                                in
                                [ if isHovered then
                                    class "bg-green-500"

                                  else
                                    class "bg-red-400"
                                , onClick <| SelectCell ( x, y )
                                , onMouseEnter <| HoverGroupFor ( x, y )
                                , onMouseLeave ResetHover
                                ]

                            else
                                []
                           )
                    )
                    [ Html.Extra.viewIf (selected == Just ( x, y )) <|
                        (groups
                            |> List.Extra.find (List.member ( x, y ))
                            |> Maybe.map List.length
                            |> Maybe.withDefault 0
                            |> String.fromInt
                            |> text
                        )
                    ]
            )
            line
