module Day01 (day) where

import Day

import Data.List (sortBy)
import Data.List.Split

parseInput :: String -> [[Int]]
parseInput = map (map (\x -> read x::Int)) . splitOn [""] . lines

calories :: [[Int]] -> [Int]
-- sums the calories and sorts them in descending order
calories = sortBy (flip compare) . map sum

solutionA :: [[Int]] -> Int
solutionA = head . calories

solutionB :: [[Int]] -> Int
solutionB = sum . take 3 . calories

solve :: FilePath -> IO ()
solve f = do
  input <- readFile f
  solA <- (pure . solutionA . parseInput) input
  solB <- (pure . solutionB . parseInput) input
  putStrLn ("The elf carrying the most calories has " ++ (show solA) ++ " calories.")
  putStrLn ("The the elves carrying the most calories have " ++ (show solB) ++ " calories in total.")

day :: Day
day = DayFile solve
  