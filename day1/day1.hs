
main :: IO ()
main = interact solve1

solve1 :: String -> String
solve1 input = show . sum $ zipWith (\a b -> if a < b then 1 else 0) parsedInput (tail parsedInput)
  where
    parsedInput = map read (lines input) :: [Int]

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xs | n > length xs = []
                   | otherwise = take n xs : slidingWindow n (tail xs)

solve2 :: String -> String
solve2 input = show . sum $ zipWith (\a b -> if a < b then 1 else 0) sums (tail sums)
  where
    parsedInput = map read (lines input)
    windows = slidingWindow 3 parsedInput
    sums = map sum windows

