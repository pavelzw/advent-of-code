module Day04 (day) where

import qualified Day
import Parse (Parser)

import Text.Megaparsec (some)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

type Section = (Int, Int)
type Pair = (Section, Section)

parseInput :: Parser [Pair]
parseInput = some parseLine

-- parseLine :: Parser Pair
-- parseLine = do
--   leftFst <- decimal
--   char '-'
--   rightFst <- decimal
--   char ','
--   leftSnd <- decimal
--   char '-'
--   rightSnd <- decimal
--   newline
--   return ((leftFst, rightFst), (leftSnd, rightSnd))

parseLine :: Parser Pair
parseLine = (,) <$> parseSection <*> (char ',' *> parseSection) <* newline
  where
    parseSection = (,) <$> decimal <*> (char '-' *> decimal)

contains :: Section -> Section -> Bool
contains (left, right) (x, y) = left <= x && y <= right

overlap :: Section -> Section -> Bool
overlap (left, right) (x, y) = not $ right < x || y < left

part1 :: [Pair] -> Int
part1 = length . filter (\(s1, s2) -> s1 `contains` s2 || s2 `contains` s1)

part2 :: [Pair] -> Int
part2 = length . filter (uncurry overlap)

day :: Day.Day [Pair] Int
day = Day.Day parseInput part1 part2
