{-# LANGUAGE OverloadedStrings #-}

module Day01 (solution, parseCalories, maxCalories) where

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

solution :: IO ()
solution = do
  input <- TIO.readFile "Day01.input.txt"
  let cals = parseCalories input
  print "Day 01: Max Calories"
  print $ maxCalories cals