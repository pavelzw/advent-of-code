module Day08 (day) where

import qualified Day
import Parse (Parser)

import Control.Applicative (liftA2)
import Data.Char (ord)
import Data.List (transpose)
import Text.Megaparsec (many, endBy)
import Text.Megaparsec.Char (newline, digitChar)

type TreeSize = Int
type Row = [TreeSize]

parseInput :: Parser [Row]
parseInput = endBy (many (flip (-) (ord '0') . ord <$> digitChar)) newline

part1 :: [Row] -> Int
part1 = sum . map fromEnum . concat . mapIndex visible

part2 :: [Row] -> Int
part2 = maximum . concat . mapIndex scenicScore

day :: Day.Day [Row] Int
day = Day.Day parseInput part1 part2

takeWhileInc :: (a -> Bool) -> [a] -> [a]
takeWhileInc p xs = if null bs then as else as ++ [head bs]
  where (as, bs) = span p xs

mapIndex :: (Int -> Int -> [[a]] -> b) -> [[a]] -> [[b]]
mapIndex f rows = [[f x y rows | x <- [0..length (rows !! y) - 1]] | y <- [0..length rows - 1]]

calculateRowColumn :: (a -> a -> a) -> (Int -> Row -> a) -> Int -> Int -> [Row] -> a
calculateRowColumn f g x y rows = f (g x (rows !! y)) (g y (transpose rows !! x))

calculateRow :: (a -> a -> a) -> (Row -> a) -> Int -> Row -> a
calculateRow f g n = liftA2 f (g . reverse . take n) (g . drop (n + 1))

scenicScore :: Int -> Int -> [Row] -> Int
scenicScore = calculateRowColumn (*) scenicScoreInRow

scenicScoreInRow :: Int -> Row -> Int
scenicScoreInRow n row = calculateRow (*) (length . takeWhileInc (< treeSize)) n row
  where
    treeSize = row !! n

visible :: Int -> Int -> [Row] -> Bool
visible = calculateRowColumn (||) visibleInRow

visibleInRow :: Int -> Row -> Bool
visibleInRow n row = calculateRow (||) (all (< treeSize)) n row
  where
    treeSize = row !! n
