import Data.List (tails, elemIndex, nub)

main :: IO ()
main = part1 >> part2

part1 :: IO ()
part1 = print . maybe 0 (+4) . elemIndex 4 . map ((length . nub) . take 4) . tails =<< readFile "Day06.txt"

part2 :: IO ()
part2 = print . maybe 0 (+14) . elemIndex 14 . map ((length . nub) . take 14) . tails =<< readFile "Day06.txt"
