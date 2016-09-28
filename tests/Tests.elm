module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import P99 exposing (..)

testMyLast : Test
testMyLast =
  describe "test myLast"
    [ test "normal" <| 
      \() -> 
        Expect.equal (Just 5) (myLast [1..5])
    , test "one" <| 
      \() -> 
        Expect.equal (Just 1) (myLast [1])

    , test "empty" <| 
      \() -> 
        Expect.equal Nothing (myLast [])
    ]

all : Test
all =
    describe "A Test Suite"
        [
        -- [ test "Addition" <|
        --     \() ->
        --         Expect.equal (3 + 7) 10
        -- , test "String.left" <|
        --     \() ->
        --         Expect.equal "a" (String.left 1 "abcdefg")
        testMyLast
        -- , test "This test should fail" <|
        --     \() ->
        --         Expect.fail "failed as expected!"
        ]
