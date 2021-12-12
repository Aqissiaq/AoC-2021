{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Data.List
import Data.Maybe

main :: IO ()
main = interact solve2

parseLine :: String -> [String]
parseLine str = (map T.unpack . (T.splitOn (T.pack "|")) . T.pack) str

solve1 :: String -> String
solve1 str = show .length $
  concatMap (\[_, output] -> filter isTargetDigit (words output)) (map parseLine (lines str))
  where
    isTargetDigit txt = elem (length txt) [2,3,4,7]

solve2 :: String -> String
solve2 str = show . sum $ concatMap (map read) correct
  where
    inOuts = [(head l, (head . reverse) l) | l <- map parseLine (lines str)]
    perms = permutations "abcdefg"
    permutedInput =
      map (\p -> [(applyPermutation p l, applyPermutation p r) | (l,r) <- inOuts]) perms
    correct = filter (not . null)
      $ map (\line -> [concatMap getDigit (words r) | (l,r) <- line,
                       all isValidDigit (words l)])
              permutedInput

validDigits = [
  "abcefg", -- 0
  "cf",     -- 1
  "acdeg",  -- 2
  "acdfg",  -- 3
  "bcdf",   -- 4
  "abdfg",  -- 5
  "abdefg", -- 6
  "acf",    -- 7
  "abcdefg",-- 8
  "abcdfg" -- 9
  ]

getDigit :: String -> String
getDigit str = show $ fromJust $ elemIndex (sort str) validDigits

isValidDigit :: String -> Bool
isValidDigit str = elem (sort str) validDigits

applyPermutation :: String -> String -> String
applyPermutation p str = map permute str
  where
    permute :: Char -> Char
    permute c = case [c] of
      "a" -> p !! 0
      "b" -> p !! 1
      "c" -> p !! 2
      "d" -> p !! 3
      "e" -> p !! 4
      "f" -> p !! 5
      "g" -> p !! 6
      " " -> ' '
      _ -> error "unknown char in permutation"
