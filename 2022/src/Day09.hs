module Day09 (day) where

import qualified Day
import Parse (Parser)

import Data.List (nub)
import Text.Megaparsec ((<|>), endBy1)
import Text.Megaparsec.Char (newline, char, space)
import Text.Megaparsec.Char.Lexer (decimal)

data Direction = R | D | L | U
type Pos = (Int, Int)
type Rope = [Pos]

parseInput :: Parser [Direction]
parseInput = concat <$> endBy1 parseDirections newline

parseDirections :: Parser [Direction]
parseDirections = flip replicate <$>
  (char 'R' *> pure R <|> char 'D' *> pure D <|> char 'L' *> pure L <|> char 'U' *> pure U)
  <*> (space *> decimal)

distance :: Pos -> Pos -> Int
-- L_infty distance
distance (x1, y1) (x2, y2) = max (abs (x1 - x2)) (abs (y1 - y2))

follow :: Pos -> Pos -> Pos
-- move one step towards dest if distance between start and end is > 1
follow (destx, desty) (srcx, srcy) =
  if distance (srcx, srcy) (destx, desty) <= 1
    then (srcx, srcy)
    else (srcx + signum (destx - srcx), srcy + signum (desty - srcy))

movePos :: Pos -> Direction -> Pos
movePos (x, y) U = (x, y + 1)
movePos (x, y) D = (x, y - 1)
movePos (x, y) R = (x + 1, y)
movePos (x, y) L = (x - 1, y)

move :: Rope -> Direction -> Rope
move rope dir = scanl follow (movePos (head rope) dir) (tail rope)

positions :: Rope -> [Direction] -> [Rope]
positions = scanl move

numberPositionsTailVisited :: Rope -> [Direction] -> Int
numberPositionsTailVisited rope = length . nub . map last . positions rope

part1 :: [Direction] -> Int
part1 = numberPositionsTailVisited [(0,0),(0,0)]

part2 :: [Direction] -> Int
part2 = numberPositionsTailVisited (replicate 10 (0,0))

day :: Day.Day [Direction] Int
day = Day.Day parseInput part1 part2
