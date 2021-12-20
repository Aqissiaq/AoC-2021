main :: IO ()
main = interact solve1

-- inputs are (move,  aim)
parseLine :: String -> (Int, Int)
parseLine line = case words line of
  ["down", n] -> (0, read n)
  ["up", n] -> (0, -(read n))
  ["forward", n] -> (read n, 0)
  _ -> error "invalid move"

prod :: (Int, Int, Int) -> Int
prod (a, b, _) = a * b

-- coords are (horizontal, depth, aim)
move :: (Int, Int, Int) -> (Int, Int) -> (Int, Int, Int)
move (x0, y0, aim0) (x, aim) = (x0 + x, y0 + (aim0 * x), aim0 + aim)

solve1 :: String -> String
solve1 input = show . prod $ foldl move (0,0,0) parsedInput
  where
    parsedInput = map parseLine (lines input)
