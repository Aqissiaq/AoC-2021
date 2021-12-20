
main :: IO ()
main = interact solve1

parseLine :: String -> (Int, Int)
parseLine line = case words line of
  ["down", n] -> (0, read n)
  ["up", n] -> (0, -(read n))
  ["forward", n] -> (read n, 0)
  _ -> error "invalid move"

prod :: (Int, Int) -> Int
prod (a, b) = a * b

addV :: (Int, Int) -> (Int, Int) -> (Int, Int)
addV (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

solve1 :: String -> String
solve1 input = show . prod $ foldl addV (0,0) parsedInput
  where
    parsedInput = map parseLine (lines input)
