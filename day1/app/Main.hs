module Main where

import Control.Monad

import Data.List
import Data.IntMap

totalDist l1 l2 = sum (zipWith (\x y -> abs (x-y)) (sort l1) (sort l2))

similarityScore l1 l2 =
  let
    l2' = fromListWith (+) (zip l2 (repeat 1))
  in
    Data.List.foldl' (\a c -> a + ((findWithDefault 0 c l2') * c)) 0 l1

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let parsedLines = Data.List.map ((\[a, b] -> (a, b)) . Data.List.map read . words) (lines contents)
  let (list1, list2) = unzip parsedLines
  print $ totalDist list1 list2
  print $ similarityScore list1 list2

  

  
