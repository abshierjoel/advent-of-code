{-# LANGUAGE RecordWildCards #-}

module Day01
  ( Direction (..),
    Turn (..),
    part1,
    part2,
    parseDirections,
  )
where

import Data.List (foldl')

maxPos, startingPos, matchingPos :: Int
maxPos = 100
startingPos = 50
matchingPos = 0

inputPath :: FilePath
inputPath = "src/day01/input.txt"

data Direction = L | R deriving (Show, Eq, Read)

data Turn = Turn {direction :: Direction, steps :: Int} deriving (Show, Eq)

data State = State
  { pos :: !Int,
    zeros :: !Int,
    matchCount :: !Int
  }
  deriving (Show)

main :: IO ()
main = do
  turns <- parseDirections <$> readFile inputPath
  putStrLn $ "Part 1: Times landed on 0: " ++ show (part1 turns)
  putStrLn $ "Part 2: Times passed through 0: " ++ show (part2 turns)

part1, part2 :: [Turn] -> Int
part1 = zeros . turn
part2 = matchCount . turn

turn :: [Turn] -> State
turn = foldl' step initialState
  where
    initialState = State startingPos 0 0

    step State {..} Turn {..} = State newPos newZeros newMatchCount
      where
        (newPos, matches) = case direction of
          L ->
            ( (pos - steps) `mod` maxPos,
              ((maxPos - pos) `mod` maxPos + steps) `div` maxPos
            )
          R ->
            ( (pos + steps) `mod` maxPos,
              (pos + steps) `div` maxPos
            )
        newMatchCount = matchCount + matches
        newZeros = zeros + fromEnum (newPos == matchingPos)

parseTurn :: String -> Turn
parseTurn s = Turn (read [head s]) (read $ tail s)

parseDirections :: String -> [Turn]
parseDirections = map parseTurn . lines
