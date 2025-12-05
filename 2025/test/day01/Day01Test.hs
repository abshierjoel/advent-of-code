module Main where

import Day01
import Test.Hspec

exampleInput :: String
exampleInput =
  unlines
    ["L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82"]

main :: IO ()
main = hspec $ do
  describe "parseDirections" $ do
    it "parses directions correctly" $ do
      let parsed = parseDirections exampleInput
          expected =
            zipWith
              Turn
              [L, L, R, L, R, L, L, L, R, L]
              [68, 30, 48, 5, 60, 55, 1, 99, 14, 82]
      parsed `shouldBe` expected

  describe "part1" $ do
    it "returns count of times landing on 0" $ do
      let result = part1 $ parseDirections exampleInput
      result `shouldBe` 3

  describe "part2" $ do
    it "returns count of times passing through 0" $ do
      let result = part2 $ parseDirections exampleInput
      result `shouldBe` 6
