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
-- move one step towards end if distance between start and end is > 1
moveTowards (srcx, srcy) (destx, desty) =
  if distance (srcx, srcy) (destx, desty) <= 1
    then (srcx, srcy)
    else (srcx + signum (destx - srcx), srcy + signum (desty - srcy))

movePos :: Pos -> Direction -> Pos
movePos (x, y) U = (x, y + 1)
movePos (x, y) D = (x, y - 1)
movePos (x, y) R = (x + 1, y)
movePos (x, y) L = (x - 1, y)

move :: HeadTailPos -> Direction -> HeadTailPos
move (h, t) dir = (h', moveTowards t h')
  where
    h' = movePos h dir

positions :: HeadTailPos -> [Direction] -> [HeadTailPos]
positions = scanl move

part1 :: [Direction] -> Int
part1 = length . nub . map snd . positions ((0,0),(0,0))

part2 :: [Direction] -> Int
part2 = undefined

day :: Day.Day [Move] Int
day = Day.Day parseInput (part1 . movesToDirections) (part2 . movesToDirections)
