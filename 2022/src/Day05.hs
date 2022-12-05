module Day05 (day) where

import qualified Day
import Parse (Parser)

import Data.List (transpose)
import Data.Char (isDigit)
import Text.Megaparsec (manyTill, try, errorBundlePretty, some, parse, between, many, takeP, (<|>), sepBy, sepBy1, endBy, endBy1, takeWhileP, satisfy)
import Text.Megaparsec.Char (char, newline, upperChar)
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Data.Text as T

type Crate = Char
type Stack = [Crate]
type Move = (Int, Int, Int)

parseStr :: Parser a -> String -> a
parseStr p s = either (error . errorBundlePretty) id (parse p "" (T.pack s))

parseLine :: Parser [Char]
parseLine = (between (char '[') (char ']') upperChar <|> between (char ' ') (char ' ') (char ' ')) `sepBy` char ' '

parseInput :: Parser ([Stack], [Move])
parseInput = do
  stacks <- endBy1 (try parseLine) newline
  takeWhileP Nothing (/= '\n')
  newline 
  newline
  moves <- endBy1 parseMoveLine newline
  let stacks' = (map (takeWhile (/= ' ') . reverse) . transpose) stacks
  return (stacks', moves)

parseMoveLine :: Parser Move
parseMoveLine = do
  string (T.pack "move ")
  n <- decimal
  string (T.pack " from ")
  from <- decimal
  string (T.pack " to ")
  to <- decimal
  return (n, from, to)

getStack :: Int -> [Stack] -> Stack
getStack n stacks = stacks !! (n - 1)

replace :: Int -> a -> [a] -> [a]
replace n x xs = take (n - 1) xs ++ [x] ++ drop n xs

move :: (Stack -> Stack) -> Move -> [Stack] -> [Stack]
move trasformer (n, from, to) stacks =
  let fromStack = getStack from stacks
      toStack = getStack to stacks
      (fromStack', toStack') = splitAt (length fromStack - n) fromStack
  in replace from fromStack' $ replace to (toStack ++ trasformer toStack') stacks

performMoves :: (Stack -> Stack) -> [Stack] -> [Move] -> [Stack]
performMoves t stacks [] = stacks
performMoves t stacks (m:ms) = performMoves t (move t m stacks) ms

part1 :: ([Stack], [Move]) -> String
part1 = map last . uncurry (performMoves reverse)

part2 :: ([Stack], [Move]) -> String
part2 = map last . uncurry (performMoves id)

day :: Day.Day ([Stack], [Move]) String
day = Day.Day parseInput part1 part2
