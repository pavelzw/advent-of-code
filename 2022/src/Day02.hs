module Day02 (day) where

import qualified Day
import Parse (Parser)

import Text.Megaparsec ((<|>), some)
import Text.Megaparsec.Char (char, space, newline)

data Shape = Rock | Paper | Scissors deriving (Eq, Enum)
instance Ord Shape where
  compare Rock Paper = LT
  compare Rock Scissors = GT
  compare Paper Rock = GT
  compare Paper Scissors = LT
  compare Scissors Rock = LT
  compare Scissors Paper = GT
  compare _ _ = EQ
data Result = Lose | Draw | Win deriving Enum
data ShapeOrResult = ShapeOrResult Shape Result

parseInput :: Parser [(Shape, ShapeOrResult)]
parseInput = some parseLine

parseLine :: Parser (Shape, ShapeOrResult)
parseLine = do
  shape <- Rock <$ char 'A' <|> Paper <$ char 'B' <|> Scissors <$ char 'C'
  _ <- space
  shapeOrResult <- ShapeOrResult Rock Lose <$ char 'X' <|> ShapeOrResult Paper Draw <$ char 'Y' <|> ShapeOrResult Scissors Win <$ char 'Z'
  _ <- newline
  return (shape, shapeOrResult)

shapeScore :: Shape -> Int
shapeScore = (+1) . fromEnum

resultScore :: Result -> Int
resultScore = (*3) . fromEnum

shapeFromResult :: Shape -> Result -> Shape
shapeFromResult opponent Lose = head $ filter ((==GT) . compare opponent) [Rock .. Scissors]
shapeFromResult opponent Draw = opponent
shapeFromResult opponent Win = head $ filter ((==LT) . compare opponent) [Rock .. Scissors]

resultFromShape :: Shape -> Shape -> Result
resultFromShape opponent shape = case compare opponent shape of
  GT -> Lose
  EQ -> Draw
  LT -> Win

calculateResult1 :: (Shape, ShapeOrResult) -> Int
calculateResult1 (opponentShape, ShapeOrResult shape _) = shapeScore shape + resultScore result
  where
  result = resultFromShape opponentShape shape

calculateResult2 :: (Shape, ShapeOrResult) -> Int
calculateResult2 (opponentShape, ShapeOrResult _ result) = shapeScore shape + resultScore result
  where
  shape = shapeFromResult opponentShape result

part1 :: [(Shape, ShapeOrResult)] -> Int
part1 = sum . map calculateResult1

part2 :: [(Shape, ShapeOrResult)] -> Int
part2 = sum . map calculateResult2

day :: Day.Day [(Shape, ShapeOrResult)] Int
day = Day.Day parseInput part1 part2
