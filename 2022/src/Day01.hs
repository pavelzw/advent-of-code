module Day01 (day) where

import qualified Day (Day(..))
import Parse (Parser)

import Data.List (sortBy)
import Text.Megaparsec (sepBy, endBy1)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal)

parseInput :: Parser [[Int]]
parseInput = sepBy (endBy1 decimal newline) newline

calories :: [[Int]] -> [Int]
calories = sortBy (flip compare) . map sum

part1 :: [[Int]] -> Int
part1 = head . calories

part2 :: [[Int]] -> Int
part2 = sum . take 3 . calories

day :: Day.Day [[Int]] Int
day = Day.Day parseInput part1 part2
