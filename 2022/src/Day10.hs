module Day10 (day) where

import qualified Day
import Parse (Parser)

import Data.List.Split (chunksOf)
import Text.Megaparsec ((<|>), endBy1)
import Text.Megaparsec.Char (newline, space, char)
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Text (pack)

data Command = Noop | AddX Int

parseInput :: Parser [Command]
parseInput = endBy1 parseCommand newline

parseCommand :: Parser Command
parseCommand = (string (pack "noop") *> pure Noop)
  <|> AddX <$> (string (pack "addx") *> space
    *> (char '-' *> ((*(-1)) <$> decimal) <|> decimal))

commandsToStates :: Int -> [Int] -> [Command] -> [Int]
commandsToStates _ states [] = states
commandsToStates current states (c:cmds) = case c of
    Noop -> commandsToStates current (states ++ [current]) cmds
    AddX x -> commandsToStates (current + x) (states ++ [current, current]) cmds

splitStates :: [Command] -> [[Int]]
splitStates = chunksOf 40 . commandsToStates 1 []

part1 :: [Command] -> String
part1 = show . sum . (zipWith (*) [20,60..]) . map (!! (20 - 1)) . splitStates

part2 :: [Command] -> String
part2 = unlines . map (zipWith (\i j -> if abs(i - j) <= 1 then '#' else '.') [0..]) . splitStates

day :: Day.Day [Command] String
day = Day.Day parseInput part1 part2
