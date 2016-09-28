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
  myReverse >> myHead

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
penultimate' =
    myReverse 
    >> myDrop 1
    >> myHead
      

elementAt : List a -> Int -> Maybe a
elementAt list index =
  if index < 0 then
    Nothing
  else 
    list |> myDrop index |> myHead

length : List a -> number
length list = 
  myFoldl (\a b -> b + 1) 0 list

palindrome : List a -> Bool
palindrome list =
  let 
    r = myReverse list
  in 
    list == r


dropWhile : (a->Bool) -> List a -> List a
dropWhile predicate list =
  case list of
    [] -> 
      list
    (x::xs) -> 
      if predicate x then
        dropWhile predicate xs
      else 
        list


  
-- util

myReverse : List a -> List a
myReverse list =
  case list of
    [] -> []
    (x::xs) -> myReverse xs ++ [x]


myDrop : Int -> List a -> List a
myDrop n list =
  case list of
    [] -> 
      list
    (x::xs) -> 
      if n > 0 then
        myDrop (n-1) xs
      else 
        list


myHead : List a -> Maybe a
myHead list =
  case list of
    [] -> Nothing
    (x::xs) -> Just x


myFoldl : (a -> b -> b) -> b -> List a -> b
myFoldl fn init list =
  case list of
    [] -> init
    (x::xs) -> myFoldl fn (fn x init) xs 


myFoldr : (a -> b -> b) -> b -> List a -> b
myFoldr fn init list =
  let 
    r = myReverse list
  in
    case r of
      [] -> init
      (x::xs) -> myFoldr fn (fn x init) xs 

