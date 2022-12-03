import Data.List (intersect, elemIndex)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)

main :: IO ()
main = part1 >> part2

part1 :: IO ()
part1 = print . sum . map (maybe 0 (+1) . (`elemIndex` (['a'..'z'] ++ ['A'..])) . head . uncurry intersect . (splitAt =<< (`div` 2) . (+1) . length)) . lines =<< readFile "Day03.txt"

part2 :: IO ()
part2 = print . sum . map (fromMaybe 0 . (`elemIndex` ('0':['a'..'z'] ++ ['A'..])) . head . foldr1 intersect) . chunksOf 3 . lines =<< readFile "Day03.txt"
