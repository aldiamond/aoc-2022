{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Day02 (solution) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import qualified Data.Text.Read as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

-- Rock, Scissors, Paper logic

type Points = Int

data Result = Win | Lose | Draw deriving (Show)

data Shape = Rock | Scissors | Paper deriving (Show)

class PointGenerator a where
  points :: a -> Points

instance PointGenerator Shape where
  points :: Shape -> Points
  points Rock = 1
  points Scissors = 2
  points Paper = 3

instance PointGenerator Result where
  points :: Result -> Points
  points Win = 6
  points Draw = 3
  points Lose = 0

result :: Shape -> Shape -> Result
result Rock Scissors = Win
result Scissors Paper = Win
result Paper Rock = Win
result Paper Paper = Draw
result Rock Rock = Draw
result Scissors Scissors = Draw
result _ _ = Lose

-- input parser

type Parser = Parsec Void T.Text

parsePlayerOne :: Parser Shape
parsePlayerOne = do
  gesture <- char 'A' <|> char 'B' <|> char 'C'
  case gesture of
    'A' -> return Rock
    'B' -> return Scissors
    'C' -> return Paper

parsePlayerTwo :: Parser Shape
parsePlayerTwo = do
  gesture <- char 'X' <|> char 'Y' <|> char 'Z'
  case gesture of
    'X' -> return Rock
    'Y' -> return Scissors
    'Z' -> return Paper

matchParser :: Parser [(Shape, Shape)]
matchParser = gameParser `sepBy` eol
  where
    gameParser = do
      player1 <- parsePlayerOne
      _ <- char ' '
      player2 <- parsePlayerTwo
      return (player1, player2)

solution :: IO ()
solution = do
  input <- TIO.readFile "data/day02.txt"
  let parseResult = parse matchParser "" input
  case parseResult of
    Right games -> do
      print "Games found"
      print games
      print $ map winner games
    Left err -> do
      print "Could not parse input"
      print err
