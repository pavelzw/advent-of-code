module Day06 (day) where

import qualified Day
import Parse (Parser)

import Text.Megaparsec (some)
import Text.Megaparsec.Char (lowerChar)
import Data.List (tails, elemIndex, nub)

type DataStream = [Char]

parseInput :: Parser DataStream
parseInput = some lowerChar

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n = map (take n) . tails

findMarker :: Int -> DataStream -> Int
findMarker n = maybe 0 (+n) . elemIndex n . map (length . nub) . slidingWindow n

part1 :: DataStream -> Int
part1 = findMarker 4

part2 :: DataStream -> Int
part2 = findMarker 14

day :: Day.Day DataStream Int
day = Day.Day parseInput part1 part2
