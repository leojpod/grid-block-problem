module Main exposing (Board, Cell(..), Group, computeGroups, main)

import Browser
import Html exposing (Html, div, span, table, td, text, tr)
import Html.Attributes exposing (class)
import Html.Attributes.Extra
import List
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


type alias Model =
    { board : Board
    , groups : List Group
    }


type alias Flags =
    ()



{-
   init zone
-}


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { board = []
      , groups = []
      }
    , generateBoard 10
    )



{-
   update and subscriptions area
-}


type Msg
    = NewBoard Board


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewBoard newBoard ->
            ( { model
                | board = newBoard

                -- TODO re-render the groups here
              }
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
    tr [] <|
        List.map
            (\cell ->
                td
                    [ Html.Attributes.Extra.attributeIf (cell == Fill) <| class "bg-red-400"
                    , class "w-12 h-12"
                    ]
                    []
            )
            line
