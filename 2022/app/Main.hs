module Main (main) where

import qualified Day
import qualified Parse as P

import Control.Applicative ((<**>))
import Data.Map (Map, fromList, (!))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Options.Applicative as OA
import qualified Text.Megaparsec as MP

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day08
import qualified Day09

solutions :: Map Int (FilePath -> IO ())
solutions = fromList
  [ (1, getProgram Day01.day)
  , (2, getProgram Day02.day)
  , (3, getProgram Day03.day)
  , (4, getProgram Day04.day)
  , (5, getProgram Day05.day)
  , (6, getProgram Day06.day)
  , (8, getProgram Day08.day)
  , (9, getProgram Day09.day)
  ]

parseOrFail :: P.Parser a -> T.Text -> a
parseOrFail p t = case MP.parse p "" t of
  Left err -> error $ MP.errorBundlePretty err
  Right x -> x

execAOC :: (Show b) => P.Parser a -> (a -> b) -> (a -> b) -> FilePath -> IO ()
execAOC parser part1 part2 inputFile = do
  source <- TIO.readFile inputFile
  let input = parseOrFail parser source
  putStrLn $ "Solution for part 1: " ++ (show . part1) input
  putStrLn $ "Solution for part 2: " ++ (show . part2) input

getProgram :: (Show b) => Day.Day a b -> (FilePath -> IO ())
getProgram (Day.Day p p1 p2) = execAOC p p1 p2

main :: IO ()
main = do
  args <- OA.execParser optsInfo
  let f = solutions ! dayOpt args
  f (inputFileOpt args)

-- argument parser
data Options = Options
  { dayOpt :: Int
  , inputFileOpt :: FilePath
  } deriving Show

opts :: OA.Parser Options
opts = Options
  <$> OA.option OA.auto
    ( OA.long "day"
    <> OA.short 'd'
    <> OA.help "Day to run" )
  <*> OA.strOption
    ( OA.long "input"
    <> OA.short 'i'
    <> OA.help "Input file" )

optsInfo :: OA.ParserInfo Options
optsInfo = OA.info (opts <**> OA.helper) (OA.fullDesc <> OA.progDesc "Advent of Code solutions")
