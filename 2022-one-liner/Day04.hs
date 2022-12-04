import Data.List.Split (splitOn)
import Data.List (isInfixOf, intersect)

main :: IO ()
main = part1 >> part2

part1 :: IO ()
part1 = print . length . filter (\x -> uncurry (flip isInfixOf) x || uncurry isInfixOf x) . map ((\x -> (head x, x!!1)) . map ((\x -> [(head x)..x!!1]) . map (\x -> read x ::Int) . splitOn "-") . splitOn ",") . lines =<< readFile "Day04.txt"

part2 :: IO ()
part2 = print . length . filter (not . null) . map (uncurry intersect . (\x -> (head x, x!!1)) . map ((\x -> [(head x)..x!!1]) . map (\x -> read x ::Int) . splitOn "-") . splitOn ",") . lines =<< readFile "Day04.txt"
