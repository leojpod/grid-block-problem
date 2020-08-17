module GroupComputation exposing (suite)

import Expect
import Main exposing (Cell(..))
import Test exposing (Test, describe, test)


type alias TestCase =
    { name : String
    , board : Main.Board
    , expectedGroups : List Main.Group
    }


emptyBoards : List TestCase
emptyBoards =
    [ { name = "0-sized board"
      , board = []
      , expectedGroups = []
      }
    , { name = "1-sized board"
      , board = [ [ Empty ] ]
      , expectedGroups = []
      }
    , { name = "4-sized board"
      , board =
            [ [ Empty, Empty, Empty, Empty ]
            , [ Empty, Empty, Empty, Empty ]
            , [ Empty, Empty, Empty, Empty ]
            , [ Empty, Empty, Empty, Empty ]
            ]
      , expectedGroups = []
      }
    ]


groupsOfOneBoard : List TestCase
groupsOfOneBoard =
    [ { name = "1-sized board"
      , board = [ [ Fill ] ]
      , expectedGroups = [ [ ( 0, 0 ) ] ]
      }
    , { name = "2-sized board"
      , board = [ [ Fill, Empty ], [ Empty, Fill ] ]
      , expectedGroups = [ [ ( 0, 0 ) ], [ ( 1, 1 ) ] ]
      }
    , { name = "4-sized board"
      , board =
            [ [ Fill, Empty, Fill, Empty ]
            , [ Empty, Fill, Empty, Fill ]
            , [ Fill, Empty, Fill, Empty ]
            , [ Empty, Fill, Empty, Fill ]
            ]
      , expectedGroups = [ [ ( 0, 0 ) ], [ ( 0, 2 ) ], [ ( 1, 1 ) ], [ ( 1, 3 ) ], [ ( 2, 0 ) ], [ ( 2, 2 ) ], [ ( 3, 1 ) ], [ ( 3, 3 ) ] ]
      }
    ]


anyKindOfGroupBoards : List TestCase
anyKindOfGroupBoards =
    [ { name = "one line in the middle"
      , board = [ [ Empty, Empty, Empty ], [ Fill, Fill, Fill ], [ Empty, Empty, Empty ] ]
      , expectedGroups = [ [ ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) ] ]
      }
    , { name = "slightly more complex"
      , board =
            [ [ Fill, Empty, Fill, Empty, Fill ]
            , [ Fill, Empty, Fill, Empty, Fill ]
            , [ Fill, Empty, Fill, Empty, Fill ]
            , [ Fill, Empty, Fill, Fill, Fill ]
            , [ Fill, Empty, Fill, Empty, Fill ]
            , [ Fill, Empty, Fill, Empty, Fill ]
            , [ Fill, Fill, Fill, Fill, Fill ]
            ]
      , expectedGroups =
            [ [ ( 0, 0 )
              , ( 0, 2 )
              , ( 0, 4 )
              , ( 1, 0 )
              , ( 1, 2 )
              , ( 1, 4 )
              , ( 2, 0 )
              , ( 2, 2 )
              , ( 2, 4 )
              , ( 3, 0 )
              , ( 3, 2 )
              , ( 3, 3 )
              , ( 3, 4 )
              , ( 4, 0 )
              , ( 4, 2 )
              , ( 4, 4 )
              , ( 5, 0 )
              , ( 5, 2 )
              , ( 5, 4 )
              , ( 6, 0 )
              , ( 6, 1 )
              , ( 6, 2 )
              , ( 6, 3 )
              , ( 6, 4 )
              ]
            ]
      }
    ]


runTest : TestCase -> Test
runTest { name, board, expectedGroups } =
    test name <|
        \_ -> Expect.equal expectedGroups <| Main.computeGroups board


suite : Test
suite =
    describe "Make sure our group computations are correct "
        [ describe "Work with empty boards" <|
            List.map runTest emptyBoards
        , describe "works with boards where there is no multi-cells groups" <|
            List.map runTest groupsOfOneBoard
        , describe "works with trickier boards" <|
            List.map runTest anyKindOfGroupBoards
        ]
