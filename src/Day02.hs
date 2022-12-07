{-# LANGUAGE OverloadedStrings #-}

module Day02 (solution) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import qualified Data.Text.Read as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void T.Text

type Points = Int

data Winner = Player1 | Player2 | Draw deriving (Show)

data Shape = Rock | Scissors | Paper deriving (Show)

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

winner :: (Shape, Shape) -> Winner
winner shapes = case shapes of
  (Rock, Scissors) -> Player1
  (Scissors, Paper) -> Player1
  (Paper, Rock) -> Player1
  (Paper, Paper) -> Draw
  (Rock, Rock) -> Draw
  (Scissors, Scissors) -> Draw
  (_, _) -> Player2

shapePoint :: Shape -> Points
shapePoint shape = case shape of
  Rock -> 1
  Scissors -> 2
  Paper -> 3

points :: (Shape, Shape) -> (Points, Points)
points (shape1, shape2) = (shapePoint shape1, shapePoint shape2)

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