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
  points Paper = 2
  points Scissors = 3

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

parseShape :: Char -> Char -> Char -> Parser Shape
parseShape a b c = do
  gesture <- char a <|> char b <|> char c
  return $ toShape gesture
  where
    toShape gesture
      | gesture == a = Rock
      | gesture == b = Paper
      | gesture == c = Scissors

matchParser :: Parser [(Shape, Shape)]
matchParser = gameParser `sepBy` eol
  where
    gameParser = do
      player1 <- parseShape 'A' 'B' 'C'
      _ <- char ' '
      player2 <- parseShape 'X' 'Y' 'Z'
      return (player1, player2)

-- solution

solution :: IO ()
solution = do
  input <- TIO.readFile "data/day02.txt"
  let parseResult = parse matchParser "" input
  case parseResult of
    Right games -> do
      print "Games found"
      let point = map (\(p1, p2) -> (points p1 + points (result p1 p2), points p2 + points (result p2 p1))) games
      print $ foldl (\(p1, p2) (p1', p2') -> (p1 + p1', p2 + p2')) (0, 0) point
    -- print $ map winner games
    Left err -> do
      print "Could not parse input"
      print err
