module Day03
  ( part1,
    part2,
    parseJoltages,
  )
where

import Data.Char (digitToInt)
import Data.List (foldl')

inputPath :: FilePath
inputPath = "src/day03/input.txt"

main :: IO ()
main = do
  banks <- parseJoltages <$> readFile inputPath
  putStrLn $ "Part 1: finding max joltages for 2 batteries: " ++ show (part1 banks)
  putStrLn $ "Part 2: finding max joltages for 12 batteries: " ++ show (part2 banks)

part1, part2 :: [[Int]] -> Int
part1 = sum . map (\bank -> fst $ findMaxNJoltages bank 2)
part2 = sum . map (\bank -> fst $ findMaxNJoltages bank 12)

findMaxNJoltages :: [Int] -> Int -> (Int, [Int])
findMaxNJoltages bank batteries =
  foldl'
    ( \(acc, rem) place ->
        let indexed = zip rem [0 ..]
            searchableRange = take (length rem - place) indexed
            (maxValue, maxIndex) = firstMaximumBy fst searchableRange
            newPlace = maxValue * (10 ^ place)
            newRem = drop (maxIndex + 1) rem
         in (acc + newPlace, newRem)
    )
    (0, bank)
    (reverse [0 .. (batteries - 1)])

firstMaximumBy :: (Ord b) => (a -> b) -> [a] -> a
firstMaximumBy f = foldl1 (\acc x -> if f x > f acc then x else acc)

parseJoltages :: String -> [[Int]]
parseJoltages = map (map digitToInt) . lines
