import Control.Monad
import Data.Char
import Data.List (findIndex, isPrefixOf)
import System.IO

-- Constants
fileName :: String
fileName = "inputB.txt"

letterWords :: [String]
letterWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

-- Main
main = do
  contents <- readFile fileName
  print . sum . map getChars . words $ contents

-- Helpers

combineDigits :: (Int, Int) -> Int
combineDigits (ones, tens) = ones + tens

getChars :: String -> Int
getChars str = do
  let tensPlace = firstB str letterWords
  let onesPlace = firstB (reverse str) (map reverse letterWords)

  (tensPlace * 10) + onesPlace

firstA :: String -> Int
firstA s@(h : t) = if isDigit h then read [h] else firstA t

firstB :: String -> [String] -> Int
firstB s@(h : t) lWords =
  if isDigit h
    then read [h]
    else case findIndex (`isPrefixOf` s) lWords of
      Nothing -> firstB t lWords
      (Just i) -> i + 1
