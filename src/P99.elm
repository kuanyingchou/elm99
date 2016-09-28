module P99 exposing (..)

import Maybe
import Debug exposing (..)

myLast : List a -> Maybe a
myLast xs = 
  case xs of
    [] -> 
      Nothing 
    [x] -> 
      Just x
    (x::xs) -> 
      myLast xs


myLast' : List a -> Maybe a
myLast' = 
  List.reverse >> List.head


penultimate : List a -> Maybe a
penultimate xs =
  case xs of
    [] -> Nothing
    [x] -> Nothing
    (x::xs) -> 
      case xs of
        [] -> Nothing
        [y] -> Just x
        otherwise -> penultimate xs


penultimate' : List a -> Maybe a
penultimate' list =
  list 
    |> List.reverse 
    |> List.drop 1
    |> List.head
      

elementAt : List a -> Int -> Maybe a
elementAt list index =
  if index < 0 then
    Nothing
  else 
    list |> List.drop index |> List.head
  



