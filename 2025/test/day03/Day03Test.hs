module Main where

import Day03
import Test.Hspec

exampleInput :: String
exampleInput = unlines
  [ "987654321111111"
  , "811111111111119"
  , "234234234234278"
  , "818181911112111"
  ]

main :: IO ()
main = hspec $ do
  describe "part1" $ do
    it "finds max joltages for 2 batteries" $ do
      let result = part1 $ parseJoltages exampleInput
      result `shouldBe` 357

  describe "part2" $ do
    it "finds max joltages for 12 batteries" $ do
      let result = part2 $ parseJoltages exampleInput
      result `shouldBe` 3121910778619
