module Main (main) where

import Data.Map
import Options.Applicative
import Day

import qualified Day01
import qualified Day02

solutions :: Map Int Day
solutions = fromList 
  [ (1, Day01.day)
  , (2, Day02.day)
  ]

runDay :: Day -> FilePath -> IO ()
runDay (DayFile f) = f
runDay (DayIO f) = const f

data Options = Options
  { day :: Int
  , input :: FilePath
  } deriving Show

opts :: Parser Options
opts = Options
  <$> option auto
    ( long "day"
    <> short 'd'
    <> help "Day to run" )
  <*> strOption
    ( long "input"
    <> short 'i'
    <> help "Input file" )

optsInfo :: ParserInfo Options
optsInfo = info (opts <**> helper) (fullDesc <> progDesc "Advent of Code solutions")

main :: IO ()
main = do
  args <- execParser optsInfo
  runDay (solutions ! day args) (input args)
