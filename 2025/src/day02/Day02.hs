module Day02
  ( part1,
    part2,
    parseRanges,
    individualNumbers,
  )
where

import Data.Char (digitToInt)

inputPath :: FilePath
inputPath = "src/day02/input.txt"

main :: IO ()
main = do
  allIds <- individualNumbers . parseRanges <$> readFile inputPath
  putStrLn $ "Part 1: strings that are mirrored: " ++ show (part1 allIds)
  putStrLn $ "Part 2: strings that have repeating patterns: " ++ show (part2 allIds)

part1, part2 :: [Int] -> Int
part1 = sum . filter doubled
part2 = sum . filter repeats

parseRanges :: String -> [(Int, Int)]
parseRanges = map parseRange . splitOn ','
  where
    parseRange s =
      let (start, rest) = break (== '-') s
       in (read start, read (tail rest))

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim s =
  let (chunk, rest) = break (== delim) s
   in chunk : case rest of
        [] -> []
        (_ : xs) -> splitOn delim xs

individualNumbers :: [(Int, Int)] -> [Int]
individualNumbers = concatMap (\(start, end) -> [start .. end])

doubled :: Int -> Bool
doubled n = firstHalf == secondHalf
  where
    ds = makeDigits n
    len = length ds
    (firstHalf, secondHalf) = splitAt (len `div` 2) ds

repeats :: Int -> Bool
repeats n
  | len < 2 = False
  | otherwise = any isPatternRepeated [1 .. maxPatternLen]
  where
    digits = makeDigits n
    len = length digits
    maxPatternLen = len `div` 2

    isPatternRepeated patternLen =
      let pattern = take patternLen digits
       in isRepeatedPattern digits pattern

isRepeatedPattern :: (Eq a) => [a] -> [a] -> Bool
isRepeatedPattern digits pattern =
  let len = length pattern in all (== pattern) (chunkEvery len digits)

-- TODO: Find Haskell equivalent to Enum.chunk_every/2
chunkEvery :: Int -> [a] -> [[a]]
chunkEvery _ [] = []
chunkEvery n list = take n list : chunkEvery n (drop n list)

makeDigits :: Int -> [Int]
makeDigits = map digitToInt . show
