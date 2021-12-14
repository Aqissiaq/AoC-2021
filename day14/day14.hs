import Data.List.Split
import qualified Data.Map as M
import Debug.Trace

main :: IO ()
main = do input <- getContents
          let [[template], rawRules] = splitOn [""] (lines input)
              -- this dumb union makes sure we count the first letter later
              poly = parsePoly template `M.union` M.singleton (' ', head template) 1
              rules = map parseRule rawRules
              letterCounts = countLetters $ iterate (applyRules rules) poly !! 40
              min = minimum $ M.elems letterCounts
              max = maximum $ M.elems letterCounts
          print $ max - min

type Pair = (Char, Char)
type Rule = (Pair, Char)
-- a polymer is actually just a bag of pairs
-- we don't care about the order
type Poly = M.Map Pair Int

parseRule :: String -> Rule
parseRule str = let [[a,b], [c]] = splitOn " -> " str in ((a,b), c)

parsePoly :: String -> Poly
parsePoly str = M.fromListWith (+) [((a,b), 1) | [a,b] <- slidingWindow 2 str]

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xs | n > length xs = []
                   | otherwise = take n xs : slidingWindow n (tail xs)

applyRule :: Poly -> Rule -> Poly
applyRule p ((a,b),c) = case M.lookup (a,b) p of
  Just n -> M.fromListWith (+) [((a,c),n), ((c,b),n)]
  Nothing -> M.empty

applyRules :: [Rule] -> Poly -> Poly
applyRules rs p = M.filter (>0) $ foldl (M.unionWith (+)) M.empty (map (applyRule p) rs)

countLetters :: Poly -> M.Map Char Int
countLetters = M.mapKeysWith (+) snd
