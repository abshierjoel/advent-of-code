module Main where

import Day02
import Test.Hspec

exampleInput :: String
exampleInput = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

main :: IO ()
main = hspec $ do
  describe "part1" $ do
    it "returns count of times landing on 0" $ do
      let result = part1 $ individualNumbers $ parseRanges exampleInput
      result `shouldBe` 1227775554

  describe "part2" $ do
    it "returns count of times passing through 0" $ do
      let result = part2 $ individualNumbers $ parseRanges exampleInput
      result `shouldBe` 4174379265
