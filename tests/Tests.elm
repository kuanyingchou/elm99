module Tests exposing (..)

import Test exposing (describe, test, Test)
import Expect exposing (Expectation)
import String
import P99 exposing (..)

all : Test
all =
  makeTestSuit 
    [ makeTestSuit 
      [ makeEqualTest (myLast [1..5]) (Just 5) 
      , makeEqualTest (myLast [1]) (Just 1) 
      , makeEqualTest (myLast []) (Nothing)
      ]
    , makeTestSuit
      [ makeEqualTest (penultimate [1..5]) (Just 4)
      , makeEqualTest (penultimate [1]) (Nothing)
      , makeEqualTest (penultimate []) (Nothing)
      , makeEqualTest (penultimate (String.toList "hello")) (Just 'l')
      ]
    , makeTestSuit
      [ makeEqualTest (penultimate' [1..5]) (Just 4)
      , makeEqualTest (penultimate' [1]) (Nothing)
      , makeEqualTest (penultimate' []) (Nothing)
      , makeEqualTest (penultimate' (String.toList "hello")) (Just 'l')
      ]
    , makeTestSuit
      [ makeEqualTest (elementAt [0..5] 3) (Just 3)
      , makeEqualTest (elementAt [0..5] 6) (Nothing)
      , makeEqualTest (elementAt [0..5] -1) (Nothing)
      ]
    , makeTestSuit
      [ makeEqualTest (length [0..5]) 6
      , makeEqualTest (length []) 0
      ]
    , makeTestSuit
      [ makeEqualTest (myReverse [1..4]) [4,3,2,1]
      , makeEqualTest (myReverse [1]) [1]
      , makeEqualTest (myReverse []) []
      ]
    , makeTestSuit
      [ makeEqualTest (palindrome [1,2,3,2,1]) True
      , makeEqualTest (palindrome [1]) True
      , makeEqualTest (palindrome [1, 1]) True
      , makeEqualTest (palindrome [1, 2, 1]) True
      , makeEqualTest (palindrome [1, 2, 2]) False
      , makeEqualTest (palindrome [2, 2, 1]) False
      , makeEqualTest (palindrome [1..5]) False
      ]
    , makeTestSuit
      [ makeEqualTest (dropWhile (\x -> x < 3) [1..5]) [3..5]
      , makeEqualTest (dropWhile (\x -> x < 0) [1..5]) [1..5]
      , makeEqualTest (dropWhile (\x -> x < 6) [1..5]) []
      ]
    , makeTestSuit
      [ makeEqualTest (takeWhile (\x -> x < 3) [1..5]) [1..2]
      , makeEqualTest (takeWhile (\x -> x < 0) [1..5]) []
      , makeEqualTest (takeWhile (\x -> x < 6) [1..5]) [1..5]
      ]
    , makeTestSuit
      [ makeEqualTest (noDupes [1,1,2,3,3,4,5,5]) [1..5]
      , makeEqualTest (noDupes [1..5]) [1..5]
      , makeEqualTest (noDupes []) []
      ]
    , makeTestSuit
      [ makeEqualTest (pack [1,1,1,2,2,3,4,4]) [[1,1,1], [2,2], [3], [4,4]]
      , makeEqualTest (pack [1]) [[1]]
      , makeEqualTest (pack []) []
      , makeEqualTest (pack [1,1,1,1]) [[1,1,1,1]]
      ]
    , makeTestSuit
      [ makeEqualTest (runLengths [['a','a','a'],  ['b']]) 
        [(3,'a'), (1,'b')]
      , makeEqualTest (runLengths []) []
      , makeEqualTest (runLengths [[]]) []
      ]
    , makeTestSuit
      [ makeEqualTest (myMap (\x -> x + 1) [1..5])  [2..6]]
    -- , makeTestSuit
    --   [ makeEqualTest (rleEncode [1,2,3,2,1]) True
    --   ]
    ]


-- util

makeEqualTest : a -> a -> Test
makeEqualTest actual expect = 
  makeEqualNotEqualTest Expect.equal actual expect


makeNotEqualTest : a -> a -> Test
makeNotEqualTest actual expect = 
  makeEqualNotEqualTest Expect.notEqual actual expect


makeEqualNotEqualTest : (a -> a -> Expectation) -> a -> a -> Test
makeEqualNotEqualTest op actual expect = 
  test "test" (\() -> op actual expect)


makeTestSuit : List Test -> Test
makeTestSuit tests =
  describe "test group" tests


