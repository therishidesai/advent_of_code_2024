module Main where

import Control.Monad

import Data.List

diffCheck (x, y) =  abs (x-y) > 0 && abs (x-y) <= 3

safeCheck l = (l == (sort l) || l == (reverse (sort l))) && all (\d -> diffCheck d) (zip l (tail l))

allLists l = aux l [l] ((length l)-1) where
  aux _ ll (-1) = ll
  aux l' ll n = let
    (s1, s2) = splitAt n l'
    nl = s1 ++ (tail s2)
    in
    aux l' ( [ nl ] ++ ll) (n-1)

safeCheckDamp l = any (\x -> x == True) (safeCheck <$> allLists l)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let ll = ((read :: String -> Int) <$>) . words <$> (lines contents)
  let s = (\l -> safeCheck l) <$> ll
  let safe = length (filter (\x -> x == True) s)
  print $ safe
  let sd = (\l -> safeCheckDamp l) <$> ll
  let safed = length (filter (\x -> x == True) sd)
  print $ safed
  
