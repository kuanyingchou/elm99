module P99 exposing (..)

import Maybe
import Debug exposing (..)

myLast : List a -> Maybe a
myLast xs = 
  case xs of
    [] -> Nothing 
    [x] -> Just x
    x::xs -> myLast xs


myLast' : List a -> Maybe a
myLast' = 
  List.reverse >> List.head

-- testMyLast : Test (Maybe a)
-- testMyLast = 
--   TestGroup [
--     Equal (myLast []) Nothing
--   -- , Equal (myLast ["a"]) (Just "a")
--   -- , Equal (myLast [3..5]) (Just 5)
--   ]
-- utilities
-- assertEq : a -> a -> Bool
-- assertEq x y =
--   if (toString x == toString y) then
--     True
--   else 
--     Debug.crash ((toString x) ++ " is not " ++ (toString y))
--
-- certain : Maybe a -> a
-- certain x =
--   case x of
--     Just a -> a
--     Nothing -> Debug.crash "There's Nothing!"
--
-- type Test comparable
--   = Equal comparable comparable
--   | NotEqual comparable comparable
--   | TestGroup (List (Test comparable))
--
-- test = 
--   TestGroup 
--     [ testMyLast
--     ]
--
-- run : Test comparable -> Bool
-- run test =
--   case test of
--     Equal x y -> 
--       (toString x) == (toString y)
--     NotEqual x y ->
--       (toString x) /= (toString y)
--     TestGroup (t::ts) ->
--       (run t) && (run (TestGroup ts))
--
--
