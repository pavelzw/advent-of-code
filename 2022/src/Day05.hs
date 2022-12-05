module Day05 (day) where

import qualified Day
import Parse (Parser)

import Data.List (transpose)
import Data.Char (isSpace)
import Text.Megaparsec (skipManyTill, anySingle, some, between, (<|>), sepBy, endBy)
import Text.Megaparsec.Char (char, newline, upperChar)
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Text (pack)

type Crate = Char
type Stack = [Crate]
data Move = Move Int Int Int

parseCrate :: Parser Crate
parseCrate = between (char '[') (char ']') upperChar <|> ' ' <$ string (pack "   ")

parseInput :: Parser ([Stack], [Move])
parseInput = do
  stacks <- map (dropWhile isSpace) . transpose <$> parseCrate `sepBy` char ' ' `endBy` newline
  _ <- skipManyTill anySingle newline *> newline
  moves <- some parseMove
  return (stacks, moves)

parseMove :: Parser Move
parseMove = Move <$> (string (pack "move ") *> decimal)
  -- use pred to convert from 1-based to 0-based indexing
  <*> (pred <$> (string (pack " from ") *> decimal))
  <*> (pred <$> (string (pack " to ") *> decimal))
  <* newline

replace :: Int -> a -> [a] -> [a]
replace n x xs = take n xs ++ [x] ++ drop (n + 1) xs

move :: (Stack -> Stack) -> [Stack] -> Move -> [Stack]
move transformer stacks (Move n from to) =
  let fromStack = stacks !! from
      toStack = stacks !! to
      (toStack', fromStack') = splitAt n fromStack
  in replace from fromStack' $ replace to (transformer toStack' ++ toStack) stacks

part1 :: ([Stack], [Move]) -> String
part1 = map head . uncurry (foldl (move reverse))

part2 :: ([Stack], [Move]) -> String
part2 = map head . uncurry (foldl (move id))

day :: Day.Day ([Stack], [Move]) String
day = Day.Day parseInput part1 part2
