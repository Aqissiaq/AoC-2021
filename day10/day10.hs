import Data.Either
import Data.List
import Control.Arrow

main :: IO ()
main = interact $ show . (solve1 &&& solve2)

solve1 :: String -> String
solve1 str = show . sum . map score1 . lefts $ map (parseLine []) (lines str)

solve2 :: String -> String
solve2 str = show . sum2 . map score2 . rights $ map (parseLine []) (lines str)

type Stack = [Char]

parseLine :: Stack -> String -> Either Char String
parseLine stack [] = Right stack
parseLine stack (c:cs)
  | c `elem` "({<[" = parseLine (c:stack) cs
  | c `matches` head stack = parseLine (tail stack) cs
  | otherwise = Left c

score1 :: Char -> Int
score1 c = case c of
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137
  _   -> error "boo"

score2 :: String -> Int
score2 = foldl (\score char -> 5*score + cScore char) 0
  where
    cScore '(' = 1
    cScore '[' = 2
    cScore '{' = 3
    cScore '<' = 4

sum2 :: [Int] -> Int
sum2 ns = (sort ns) !! ((length ns) `div` 2)

matches :: Char -> Char -> Bool
matches closing opening = [opening, closing] `elem` ["()","<>","{}","[]"]
