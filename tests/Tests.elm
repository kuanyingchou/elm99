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


