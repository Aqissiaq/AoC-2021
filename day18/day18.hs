import Data.Char
import Data.Maybe
import Data.Tuple.Extra
import Control.Applicative

data Snum = Lit Int | Pair Snum Snum
  deriving(Show)

main :: IO ()
main = do input <- getContents
          let snums = map parse (lines input)
          -- part 1
          print . magnitude $ foldl1 sAdd snums
          -- part 2
          print $ maximum [magnitude $ x `sAdd` y | x <- snums, y <- snums]

sAdd :: Snum -> Snum -> Snum
sAdd = (reduce .) . Pair

reduce :: Snum -> Snum
reduce sn = maybe sn reduce (explode sn <|> split sn)

explode :: Snum -> Maybe Snum
explode s = explode' 0 s >>= Just . fst3
  where
    explode' :: Int -> Snum -> Maybe (Snum, Int, Int)
    explode' 4 (Pair (Lit x) (Lit y)) = Just (Lit 0, x, y)
    explode' depth (Pair l r) =
      case explode' (depth + 1) l of
        Just (newL, x, y) -> Just (Pair newL (addToLeftMost y r), x, 0)
        Nothing -> explode' (depth + 1) r
                   >>= (\(newR, x, y) -> Just (Pair (addToRightMost x l) newR, 0, y))
    explode' _ _ = Nothing

addToLeftMost, addToRightMost :: Int -> Snum -> Snum
addToLeftMost x (Lit y) = Lit (x+y)
addToLeftMost x (Pair l r) = Pair (addToLeftMost x l) r

addToRightMost x (Lit y) = Lit (x+y)
addToRightMost x (Pair l r) = Pair l (addToRightMost x r)


split :: Snum -> Maybe Snum
split (Lit n) | n >= 10 = Just $ Pair (Lit $ n `div` 2) (Lit $ (n+1) `div` 2)
              | otherwise = Nothing
split (Pair l r) = case split l of
  Just newL -> Just (Pair newL r)
  Nothing -> split r >>= Just . Pair l

magnitude :: Snum -> Int
magnitude (Lit n) = n
magnitude (Pair l r) = (magnitude l * 3) + (magnitude r * 2)

parseSnum :: String -> (Snum, String)
parseSnum ('[':rest)= let (left, leftRest) = parseSnum rest
                          (right, rightRest) = parseSnum $ tail leftRest in
                        (Pair left right, tail rightRest)
parseSnum (x:rest) = (Lit $ digitToInt x, rest)
parseSnum e = error $ "Cannot parse: " ++ show e

parse = fst . parseSnum
