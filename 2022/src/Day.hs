module Day ( Day(..) ) where

import Parse (Parser)

data Day a b = Day { parser :: Parser a, part1 :: a -> b, part2 :: a -> b }
