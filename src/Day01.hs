{-# LANGUAGE OverloadedStrings #-}

module Day01 (solution, parseCalories, maxCalories, top) where

import Data.List (sort, sortBy)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import qualified Data.Text.Read as T

parseCalories :: T.Text -> [[Integer]]
parseCalories = fmap (fmap parseInt . T.lines) . T.splitOn (T.pack "\n\n")
  where
    parseInt :: T.Text -> Integer
    parseInt = either error fst . T.decimal

maxCalories :: [[Integer]] -> Integer
maxCalories = foldl max (0 :: Integer) . map sum

top :: Int -> [[Integer]] -> [Integer]
top n = take n . sortBy (flip compare) . map sum

solution :: IO ()
solution = do
  input <- TIO.readFile "data/day01.txt"
  let calories = parseCalories input
  print "Day 01: Max Calories"
  print $ maxCalories calories
  print "Day 01 (prt2): sum top 3 elf calories"
  print $ sum . top 3 $ calories