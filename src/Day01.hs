{-# LANGUAGE OverloadedStrings #-}

module Day01 (solution, parseCalories, maxCalories, top) where

import Data.List (sort, sortBy)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import qualified Data.Text.Read as T

parseCalories :: T.Text -> Either String [[Integer]]
parseCalories = mapM (mapM parseInt . T.lines) . T.splitOn (T.pack "\n\n")
  where
    parseInt :: T.Text -> Either String Integer
    parseInt = fmap fst . T.decimal

maxCalories :: [[Integer]] -> Integer
maxCalories = foldl max (0 :: Integer) . map sum

top :: Int -> [[Integer]] -> [Integer]
top n = take n . sortBy (flip compare) . map sum

solution :: IO ()
solution = do
  input <- TIO.readFile "data/day01.txt"
  let maybeCalories = parseCalories input
  case maybeCalories of
    Right calories -> do
      print "Day 01: Max Calories"
      print $ maxCalories calories
      print "Day 01 (prt2): sum top 3 elf calories"
      print $ sum $ top 3 calories
    Left err -> do
      print "Could not parse input"
      print err