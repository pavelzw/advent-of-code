module Day02 (day) where

import Day

import Data.Char (isSpace)

data RPS = Rock | Paper | Scissors deriving (Eq, Show)
data Result = Win | Lose | Draw deriving (Eq, Show)
data Game = Game RPS RPS deriving (Eq, Show)

parseOpponent :: Char -> RPS
parseOpponent 'A' = Rock
parseOpponent 'B' = Paper
parseOpponent 'C' = Scissors
parseOpponent _ = error "Invalid opponent input"

parseSuggestionA :: Char -> RPS
parseSuggestionA 'X' = Rock
parseSuggestionA 'Y' = Paper
parseSuggestionA 'Z' = Scissors
parseSuggestionA _ = error "Invalid suggestion input"

parseSuggestionB :: Char -> Result
parseSuggestionB 'X' = Lose
parseSuggestionB 'Y' = Draw
parseSuggestionB 'Z' = Win
parseSuggestionB _ = error "Invalid suggestion input"

parseInput :: ([Char] -> Game) -> String -> [Game]
parseInput parseLine = map (parseLine . filter (not . isSpace)) . lines

parseLineA :: [Char] -> Game
parseLineA [a, b] = Game (parseOpponent a) (parseSuggestionA b)
parseLineA _ = error "Invalid input line"

parseLineB :: [Char] -> Game
parseLineB [a, b] = Game (parseOpponent a) (calculateSuggestion (parseOpponent a) (parseSuggestionB b))
parseLineB _ = error "Invalid input line"

calculateSuggestion :: RPS -> Result -> RPS
calculateSuggestion Rock Win = Paper
calculateSuggestion Paper Win = Scissors
calculateSuggestion Scissors Win = Rock
calculateSuggestion Rock Lose = Scissors
calculateSuggestion Paper Lose = Rock
calculateSuggestion Scissors Lose = Paper
calculateSuggestion a Draw = a

calculateResult :: Game -> Result
calculateResult (Game Rock Paper) = Win
calculateResult (Game Rock Scissors) = Lose
calculateResult (Game Paper Rock) = Lose
calculateResult (Game Paper Scissors) = Win
calculateResult (Game Scissors Rock) = Win
calculateResult (Game Scissors Paper) = Lose
calculateResult (Game _ _) = Draw

calculateShapeScore :: Game -> Int
calculateShapeScore (Game _ Rock) = 1
calculateShapeScore (Game _ Paper) = 2
calculateShapeScore (Game _ Scissors) = 3

calculateScore :: Game -> Int
calculateScore g =
  let s = calculateShapeScore g in
  s + case calculateResult g of
  Win -> 6
  Lose -> 0
  Draw -> 3

solution :: [Game] -> Int
solution = sum . map calculateScore

solve :: FilePath -> IO ()
solve f = do
  input <- readFile f
  solA <- (pure . solution . (parseInput parseLineA)) input
  solB <- (pure . solution . (parseInput parseLineB)) input
  putStrLn ("The total score if everthing goes exactly according to the strategy guide is " ++ (show solA) ++ ".")
  putStrLn ("The total score if everthing goes exactly according to the elf's instructions is " ++ (show solB) ++ ".")

day :: Day
day = DayFile solve
