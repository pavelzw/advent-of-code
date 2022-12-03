import Data.List.Split (splitOn)
import Data.List (sort)

main :: IO ()
main = part1 >> part2

part1 :: IO ()
part1 = print . maximum . map (sum . map (\x -> read x::Int)) . splitOn [""] . lines =<< readFile "Day01.txt"

part2 :: IO ()
part2 = print . sum . take 3 . reverse . sort . map (sum . map (\x -> read x::Int)) . splitOn [""] . lines =<< readFile "Day01.txt"
