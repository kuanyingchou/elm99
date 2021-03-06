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

    , makeTestSuit
      [ makeEqualTest (rleEncode [1,1,1,2,2,3,4,4]) [ Run 3 1, Run 2 2, Single 3, Run 2 4 ] 
      , makeEqualTest (rleEncode [1,1,1,1,1]) [ Run 5 1 ] 
      , makeEqualTest (rleEncode [1]) [ Single 1 ] 
      , makeEqualTest (rleEncode ['a']) [ Single 'a' ] 
      ]
    , makeTestSuit
      [ makeEqualTest (rleDecode [ Run 3 1, Run 2 2, Single 3, Run 2 4 ]) [1,1,1,2,2,3,4,4] 
      , makeEqualTest (rleDecode [ Run 5 1 ]) [1,1,1,1,1]  
      , makeEqualTest (rleDecode [ Single 1 ]) [1]
      , makeEqualTest (rleDecode [ Single 'a' ]) ['a']
      ]
    , makeTestSuit
      [ makeEqualTest (duplicate [1,2,3,5,8,8]) [1,1,2,2,3,3,5,5,8,8,8,8]
      , makeEqualTest (duplicate ['a', 'b']) ['a','a','b','b']
      , makeEqualTest (duplicate []) []
      ]
    , makeTestSuit
      [ makeEqualTest (repeatElements 3 [1,2]) [1,1,1,2,2,2]
      ]
    , makeTestSuit
      [ makeEqualTest (dropNth [1..10] 3) [1,2,4,5,7,8,10]
      , makeEqualTest (dropNth [1,1,2,3,3,3] 2) [1,2,3]
      , makeEqualTest (dropNth [] 2) []
      , makeEqualTest (dropNth [1..10] 1) []
      , makeEqualTest (dropNth [1..10] 11) [1..10]
      ]
    , makeTestSuit
      [ makeEqualTest (split [1..10] 3) ([1..3], [4..10])
      , makeEqualTest (split [1..5] 0) ([], [1, 2, 3, 4, 5])
      , makeEqualTest (split [1..5] 2) ([1, 2], [3, 4, 5]) 
      , makeEqualTest (split [1..5] 3) ([1, 2, 3], [4, 5])
      , makeEqualTest (split [1..5] 4) ([1, 2, 3, 4], [5]) 
      , makeEqualTest (split [1..5] 5) ([1, 2, 3, 4, 5], []) 
      , makeEqualTest (split [1..5] 6) ([1, 2, 3, 4, 5], [])
      , makeEqualTest (split [1..5] (-1)) ([], [1, 2, 3, 4, 5]) 
      , makeEqualTest (split [ "aab", "b", "c", "aa" ] 2)  
          ([ "aab", "b"],["c", "aa" ])
      ]
    , makeTestSuit
      [ makeEqualTest (myTake 3 [1..10]) [1..3]
      , makeEqualTest (myTake 0 [1..10]) []
      , makeEqualTest (myTake -1 [1..10]) []
      , makeEqualTest (myTake 11 [1..10]) [1..10]
      ]
    , makeTestSuit
      [ makeEqualTest (slice 3 7 [1..10]) [3..7]
      , makeEqualTest (slice 2 100 [ 'a', 'b', 'c' ]) [ 'b', 'c' ]
      , makeEqualTest (slice -1 2 [0..100]) [0, 1]
      , makeEqualTest (slice -3 -2 [-3, -2, -1, 0, 1, 2, 3]) []
      , makeEqualTest (slice 5 3 [ "indices", " are", "inverted"]) []
      , makeEqualTest (slice 0 1 [0..10]) [0]
      ]
    , makeTestSuit
      [ makeEqualTest (rotate 3 [ 1, 2, 5, 5, 2, 1 ]) [ 5, 2, 1, 1, 2, 5 ]
      , makeEqualTest (rotate 13 [ 1..10 ]) [ 4, 5, 6, 7, 8, 9, 10, 1, 2, 3 ]
      , makeEqualTest (rotate 1 [1..5]) [ 2, 3, 4, 5, 1 ] -- rotate left
      , makeEqualTest (rotate 0 [1..5]) [ 1, 2, 3, 4, 5 ]
      , makeEqualTest (rotate -1 [1..5]) [ 5, 1, 2, 3, 4 ] -- rotate right
      , makeEqualTest (rotate -6 [1..5]) [ 5, 1, 2, 3, 4 ]
      , makeEqualTest (rotate 3 [1..5]) [ 4, 5, 1, 2, 3 ]
      , makeEqualTest (rotate 1 [ "1", "2", "3", "4" ]) [ "2", "3", "4", "1" ]
      ]
    , makeTestSuit
      [ makeEqualTest (dropAt 2 [ 1, 2, 5, 5, 2, 1 ]) [ 1, 5, 5, 2, 1 ]
      , makeEqualTest (dropAt 3 [1..14]) [ 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 ]
      , makeEqualTest (dropAt 6 [1..5]) [ 1, 2, 3, 4, 5 ]
      , makeEqualTest (dropAt 0 [1..5]) [ 1, 2, 3, 4, 5 ]
      , makeEqualTest (dropAt -1 [1..5]) [ 1, 2, 3, 4, 5 ]
      , makeEqualTest (dropAt 1 [1..5]) [ 2, 3, 4, 5 ]
      , makeEqualTest (dropAt 2 [ "1", "2", "3", "4", "5" ]) [ "1", "3", "4", "5" ]
      ]
    , makeTestSuit
      [ makeEqualTest (insertAt 2 99 [ 1, 2, 5, 5, 2, 1 ]) [ 1, 99, 2, 5, 5, 2, 1 ]
      , makeEqualTest (insertAt 3 99 [1..14]) [ 1, 2, 99, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 ]
      , makeEqualTest (insertAt 6 99 [1..5]) [ 1, 2, 3, 4, 5, 99 ]
      , makeEqualTest (insertAt 0 99 [1..5]) [ 99, 1, 2, 3, 4, 5 ]
      , makeEqualTest (insertAt -1 99 [1..5]) [ 99, 1, 2, 3, 4, 5 ]
      , makeEqualTest (insertAt 1 99 [1..5]) [ 99, 1, 2, 3, 4, 5 ]
      , makeEqualTest (insertAt 2 "x" [ "1", "2", "3", "4", "5" ]) [ "1", "x", "2", "3", "4", "5" ]
      ]
    , makeTestSuit
      [ makeEqualTest (range 1 5) [1..5]
      , makeEqualTest (range 0 5) [0..5]
      , makeEqualTest (range -1 5) [-1..5]
      , makeEqualTest (range 5 -1) [ 5, 4, 3, 2, 1, 0, -1 ]
      , makeEqualTest (range 5 5) [ 5 ]
      , makeEqualTest (List.length (range 1 999)) 999
      ]
    , makeTestSuit
      [ makeEqualTest (range' 1 5) [1..5]
      , makeEqualTest (range' 0 5) [0..5]
      , makeEqualTest (range' -1 5) [-1..5]
      , makeEqualTest (range' 5 -1) [ 5, 4, 3, 2, 1, 0, -1 ]
      , makeEqualTest (range' 5 5) [ 5 ]
      , makeEqualTest (List.length (range' 1 999)) 999
      ]
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


