module Day03 (day) where

import qualified Day
import Parse (Parser)

import qualified Data.Char as C (isLower, ord)
import Data.List (intersect)
import Text.Megaparsec (many, manyTill)
import Text.Megaparsec.Char (newline, letterChar)

parseInput :: Parser [String]
parseInput = many (manyTill letterChar newline)

ord :: Char -> Int
ord c = if C.isLower c then
    C.ord c - C.ord 'a' + 1
else
    C.ord c - C.ord 'A' + 27

part1 :: [String] -> Int
part1 = sum . map (ord . head . \s -> uncurry intersect (splitAt ((length s + 1) `div` 2) s))

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null) . map (take n) . iterate (drop n)

part2 :: [String] -> Int
part2 = sum . map (ord . head . foldr1 intersect) . splitEvery 3

day :: Day.Day [String] Int
day = Day.Day parseInput part1 part2
