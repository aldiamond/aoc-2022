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

inputFile :: String
inputFile = "data/day02.txt"

-- Rock, Scissors, Paper logic

type Points = Int

data Result = Win | Lose | Draw deriving (Show)

data Shape = Rock | Paper | Scissors deriving (Show)

alt :: Result -> Result
alt Win = Lose
alt Draw = Draw
alt Lose = Win

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
        (Paper, Lose) -> Scissors
        (Rock, Lose) -> Paper
        (Scissors, Lose) -> Rock
        (shape1, Draw) -> shape1

-- input parser

type PlayerCode = Map.Map Char

playerSubstitutions :: (MatchResolver a) => [(Char, a)] -> PlayerCode a
playerSubstitutions = Map.fromList

type Parser = Parsec Void T.Text

parseMatchToken :: PlayerCode a -> Parser a
parseMatchToken tokens = do
  gesture <- choice $ map char $ keys tokens
  return $ tokens ! gesture

matchParser :: PlayerCode a -> PlayerCode b -> Parser [(a, b)]
matchParser p1Tokens p2Tokens = gameParser `sepBy` eol
  where
    gameParser = do
      player1 <- parseMatchToken p1Tokens
      _ <- char ' '
      player2 <- parseMatchToken p2Tokens
      return (player1, player2)

-- solution

tallyWinner :: (MatchResolver a) => PlayerCode Shape -> PlayerCode a -> IO (Points, Points)
tallyWinner player1Code player2Code = do
  input <- TIO.readFile inputFile
  let parseResult = parse (matchParser player1Code player2Code) "" input
  case parseResult of
    Right games -> do
      let matchResults = map (uncurry resolve) games
      let matchPoints = map (\(shape1, shape2, res) -> (points shape1 + points res, points shape2 + points (alt res))) matchResults
      let totalPoints = zipSum matchPoints
      return totalPoints
    Left err -> do
      print "Could not parse input"
      print $ show err
      return (0, 0)

solution :: IO ()
solution = do
  print "========================================"
  print "Part 01"
  resPrt1 <-
    tallyWinner
      (playerSubstitutions [('A', Rock), ('B', Paper), ('C', Scissors)])
      (playerSubstitutions [('X', Rock), ('Y', Paper), ('Z', Scissors)])
  print "Total Points"
  print resPrt1
  print "========================================"
  print "Part 02"
  resPrt2 <-
    tallyWinner
      (playerSubstitutions [('A', Rock), ('B', Paper), ('C', Scissors)])
      (playerSubstitutions [('X', Win), ('Y', Draw), ('Z', Lose)])
  print "Total Points"
  print resPrt2

-- helpers

zipSum :: (Num a, Num b) => [(a, b)] -> (a, b)
zipSum xs = (sum (map fst xs), sum (map snd xs))