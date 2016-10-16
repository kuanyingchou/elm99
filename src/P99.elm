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


takeWhile : (a->Bool) -> List a -> List a
takeWhile predicate list = 
  case list of
    [] -> 
      list
    (x::xs) -> 
      if predicate x then
        x :: takeWhile predicate xs
      else 
        []


noDupes : List a -> List a
noDupes list =
  case list of
    [] -> 
      []

    [x] -> 
      [x]

    x::y::rest -> 
      if x == y then
        noDupes (y::rest)
      else 
        x :: (noDupes (y::rest))


-- !!!
pack : List a -> List (List a)
pack list =
  case list of

    [] -> 
      []

    otherwise ->
      let 
        separate first rest = 
          case rest of
            [] -> 
              (first, [])

            x::xs -> 
              case first of
                [] -> 
                  separate [x] xs

                y::ys ->
                  if x == y then
                    separate (y::first) xs
                  else 
                    (first, rest)
        (first, rest) = separate [] list
      in
        first :: pack rest


runLengths : List (List a) -> List (Int, a)
runLengths list =
  let 
    convert list = 
      case list of
        [] -> 
          Debug.crash "should not be empty"

        x::xs ->
          (myLength list, x)

    filter list =
      case list of
        [] -> False
        x::xs -> True
  in 
    myMap convert (myFilter filter list)


type RleCode a = Run Int a | Single a

rleEncode : List a -> List (RleCode a)
rleEncode list =
  case list of
    [] ->
      []
    [x] -> 
      [ Single x ]
    x::xs ->
      rleEncodeHelper [] (Single x) xs |> myReverse

rleEncodeHelper : List (RleCode a) -> RleCode a -> List a -> List (RleCode a)
rleEncodeHelper fixed first rest = 
  let 
    (c, e) =
      case first of
        Single x ->
          (1, x)
        Run count x ->
          (count, x)
  in
    case rest of
      [] -> 
        first :: fixed

      x :: xs ->
        if x == e then
          rleEncodeHelper fixed (Run (c+1) e) xs
        else
          rleEncodeHelper (first :: fixed) (Single x) xs
           

rleDecode : List (RleCode a) -> List a 
rleDecode list = 
  case list of
    [] ->
      []

    x::xs ->
      case x of
        Single x ->
          x :: rleDecode xs
        Run count value ->
          -- (myRepeat count value) ++ (rleDecode xs)
          if count == 1 then
            value :: (rleDecode xs)
          else 
            if count == 2 then
              value :: rleDecode ((Single value) :: xs)
            else
              value :: rleDecode ((Run (count-1) value) :: xs)

duplicate : List a -> List a
duplicate list =
  case list of
    [] ->
      []
    x::xs ->
      x :: x :: duplicate xs
  
repeatElements : Int -> List a -> List a
repeatElements count list =
  case list of
    [] ->
      []
    x :: xs ->
      myRepeat count x ++ repeatElements count xs

dropNth : List a -> Int -> List a
dropNth list n =
  let 
    dropNthHelper list m n =
      case list of
        [] ->
          []
        x :: xs ->
          if m % n == 0 then
            dropNthHelper xs (m+1) n
          else
            x :: dropNthHelper xs (m+1) n
  in 
    dropNthHelper list 1 n


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


myMap : (a -> b) -> List a -> List b
myMap func xs =
  case xs of
    [] -> []
    x::xs -> 
      func x :: myMap func xs 
  
myLength : List a -> Int
myLength list =
  case list of
    [] -> 0
    x::xs -> 1 + myLength xs

myFilter : (a -> Bool) -> List a -> List a
myFilter predicate list =
  case list of
    [] -> 
      []
    x::xs -> 
      let 
        rest = myFilter predicate xs
      in 
        if predicate x then
          x :: rest
        else
          rest

myRepeat : Int -> a -> List a
myRepeat count x =
  if count == 0 then
    []
  else 
    x :: myRepeat (count-1) x

split : List a -> Int -> (List a, List a)
split list count =
  let 
    splitHelper list count pre =
      case list of
        [] ->
          (pre, [])
        x::xs ->
          if count > 0 then
            splitHelper xs (count-1) (x::pre)
          else 
            (pre, list)

    (left, right) = splitHelper list count []
  in 
    (myReverse left, right)

slice : Int -> Int -> List a -> List a
slice from to list =
  let 
    toDrop = 
      if from <= 0 then
        0
      else 
        from - 1
    toTake = 
      if to <= 0 then
        0
      else 
        to - toDrop
  in
    list |> myDrop toDrop |> myTake toTake

myTake : Int -> List a -> List a
myTake toTake list =
  let 
    helper toTake taken list =
      case list of
        [] ->
          taken
        x :: xs ->
          if toTake > 0 then
            helper (toTake-1) (x :: taken) xs
          else 
            taken

  in 
     (helper toTake [] list) |> myReverse

-- 1 2 3 4 5
-- 2 3 4 5 1 
-- 3 4 5 1 2
rotate : Int -> List a -> List a
rotate count list =
  let 
    len = length list
    c = (abs count) % len
  in
    if count >= 0 then -- go left
      myDrop c list ++ myTake c list
    else -- go right
      myDrop (len-c) list ++ myTake (len-c) list


dropAt : Int -> List a -> List a
dropAt index list =
  if index < 0 then
    list
  else 
    (myTake (index-1) list) ++ (myDrop index list)


insertAt : Int -> a -> List a -> List a
insertAt index element list =
  myTake (index-1) list ++ [element] ++ myDrop (index-1) list

range : Int -> Int -> List Int
range begin end =
  if begin < end then
    [begin..end]
  else 
    myReverse [end..begin]

range' : Int -> Int -> List Int
range' begin end =
  if begin < end then
    begin :: range' (begin+1) end
  else if begin > end then
    begin :: range' (begin-1) end
  else
    [begin]



