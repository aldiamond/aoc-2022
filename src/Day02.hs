{-# LANGUAGE OverloadedStrings #-}

module Day02 (solution) where

import Data.Map (keys, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import qualified Data.Text.Read as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

-- Rock, Scissors, Paper logic

type Points = Int

data Result = Win | Lose | Draw deriving (Show)

data Shape = Rock | Paper | Scissors deriving (Show)

class PointGenerator a where
  points :: a -> Points

instance PointGenerator Shape where
  points Rock = 1
  points Paper = 2
  points Scissors = 3

instance PointGenerator Result where
  points Win = 6
  points Draw = 3
  points Lose = 0

class MatchResolver a where
  resolve :: Shape -> a -> (Shape, Shape, Result)

instance MatchResolver Shape where
  resolve shape1 shape2 = (shape1, shape2, result)
    where
      result = case (shape1, shape2) of
        (Rock, Scissors) -> Win
        (Scissors, Paper) -> Win
        (Paper, Rock) -> Win
        (Paper, Paper) -> Draw
        (Rock, Rock) -> Draw
        (Scissors, Scissors) -> Draw
        (shape1, shape2) -> Lose

instance MatchResolver Result where
  resolve shape1 result = (shape1, shape2, result)
    where
      shape2 = case (shape1, result) of
        (Rock, Win) -> Scissors
        (Scissors, Win) -> Paper
        (Paper, Win) -> Rock
        (Paper, Lose) -> Paper
        (Rock, Lose) -> Rock
        (Scissors, lose) -> Scissors
        (shape1, Draw) -> shape1

-- input parser

type Parser = Parsec Void T.Text

parseMatchToken :: Map.Map Char a -> Parser a
parseMatchToken tokens = do
  gesture <- choice $ map char $ keys tokens
  return $ tokens ! gesture

matchParser :: Map.Map Char a -> Map.Map Char b -> Parser [(a, b)]
matchParser p1Tokens p2Tokens = gameParser `sepBy` eol
  where
    gameParser = do
      player1 <- parseMatchToken p1Tokens
      _ <- char ' '
      player2 <- parseMatchToken p2Tokens
      return (player1, player2)

-- solution

solution :: IO ()
solution = do
  input <- TIO.readFile "data/day02.txt"
  let p1Tokens = Map.fromList [('A', Rock), ('B', Paper), ('C', Scissors)]
  let p2Tokens = Map.fromList [('X', Rock), ('Y', Paper), ('Z', Scissors)]
  let parseResult = parse (matchParser p1Tokens p2Tokens) "" input
  case parseResult of
    Right games -> do
      print "Games found"
      let point = map (\(p1, p2) -> (points p1 + points (result p1 p2), points p2 + points (result p2 p1))) games
      print $ foldl (\(p1, p2) (p1', p2') -> (p1 + p1', p2 + p2')) (0, 0) point
    -- print $ map winner games
    Left err -> do
      print "Could not parse input"
      print err
