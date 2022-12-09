module Day09 (day) where

import qualified Day
import Parse (Parser)

import Data.List (nub)
import Text.Megaparsec (endBy1, (<|>))
import Text.Megaparsec.Char (newline, char, space)
import Text.Megaparsec.Char.Lexer (decimal)

data Direction = R | D | L | U
  deriving (Show)
data Move = Move Direction Int deriving (Show)

type Pos = (Int, Int)
type HeadTailPos = (Pos, Pos)

type RopePos = [Pos]

parseInput :: Parser [Move]
parseInput = endBy1 (Move R <$> (char 'R' *> space *> decimal) <|> Move D <$> (char 'D' *> space *> decimal) <|> Move L <$> (char 'L' *> space *> decimal) <|> Move U <$> (char 'U' *> space *> decimal)) newline

movesToDirections :: [Move] -> [Direction]
movesToDirections = concat . map (\(Move dir n) -> replicate n dir)

distance :: Pos -> Pos -> Int
-- L_infty distance
distance (x1, y1) (x2, y2) = max (abs (x1 - x2)) (abs (y1 - y2))

moveTowards :: Pos -> Pos -> Pos
-- moveTowards start end
moveTowards (srcx, srcy) (destx, desty) = (srcx + signum (destx - srcx), srcy + signum (desty - srcy))

moveOne :: HeadTailPos -> Direction -> HeadTailPos
moveOne (head, tail) U = (head', tail')
  where
    head' = (fst head, snd head + 1)
    tail' = if distance head' tail <= 1 then tail else moveTowards tail head'
moveOne (head, tail) D = (head', tail')
  where
    head' = (fst head, snd head - 1)
    tail' = if distance head' tail <= 1 then tail else moveTowards tail head'
moveOne (head, tail) R = (head', tail')
  where
    head' = (fst head + 1, snd head)
    tail' = if distance head' tail <= 1 then tail else moveTowards tail head'
moveOne (head, tail) L = (head', tail')
  where
    head' = (fst head - 1, snd head)
    tail' = if distance head' tail <= 1 then tail else moveTowards tail head'

positions :: HeadTailPos -> [Direction] -> [HeadTailPos]
positions = scanl moveOne

part1 :: [Move] -> Int
part1 = length . nub . map snd . positions ((0,0),(0,0)) . movesToDirections

part2 :: [Move] -> Int
part2 = undefined

day :: Day.Day [Move] Int
day = Day.Day parseInput part1 part2
