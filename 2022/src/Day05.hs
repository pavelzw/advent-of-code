module Day05 (day) where

import qualified Day
import Parse (Parser)

import Data.List (transpose)
import Data.Char (isSpace)
import Text.Megaparsec (skipManyTill, anySingle, errorBundlePretty, some, parse, between, (<|>), sepBy, endBy)
import Text.Megaparsec.Char (char, newline, upperChar)
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Text (pack)

type Crate = Char
type Stack = [Crate]
data Move = Move Int Int Int deriving Show

parseStr :: Parser a -> String -> a
parseStr p s = either (error . errorBundlePretty) id (parse p "" (pack s))

parseCrate :: Parser Crate
parseCrate = between (char '[') (char ']') upperChar <|> ' ' <$ string (pack "   ")

parseInput :: Parser ([Stack], [Move])
parseInput = do
  stacks <- map (reverse . dropWhile isSpace) . transpose <$> parseCrate `sepBy` char ' ' `endBy` newline
  skipManyTill anySingle newline <* newline
  moves <- some parseMove
  return (stacks, moves)

parseMove :: Parser Move
parseMove = Move <$> (string (pack "move ") *> decimal) <*> (pred <$> (string (pack " from ") *> decimal)) <*> (pred <$> (string (pack " to ") *> decimal)) <* newline

replace :: Int -> a -> [a] -> [a]
replace n x xs = take n xs ++ [x] ++ drop (n + 1) xs

move :: (Stack -> Stack) -> Move -> [Stack] -> [Stack]
move trasformer (Move n from to) stacks =
  let fromStack = stacks !! from
      toStack = stacks !! to
      (fromStack', toStack') = splitAt (length fromStack - n) fromStack
  in replace from fromStack' $ replace to (toStack ++ trasformer toStack') stacks

performMoves :: (Stack -> Stack) -> [Stack] -> [Move] -> [Stack]
performMoves _ stacks [] = stacks
performMoves t stacks (m:ms) = performMoves t (move t m stacks) ms

part1 :: ([Stack], [Move]) -> String
part1 = map last . uncurry (performMoves reverse)

part2 :: ([Stack], [Move]) -> String
part2 = map last . uncurry (performMoves id)

day :: Day.Day ([Stack], [Move]) String
day = Day.Day parseInput part1 part2
