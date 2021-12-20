import Data.Char
import Data.Bool

main :: IO ()
main = interact solve2

type Bin = [Int]

solve1 :: String -> String
solve1 str = show $ gamma input * epsilon input
  where
    input = map (map digitToInt) (lines str)

binToInt :: Bin -> Int
binToInt = foldl ((+) . (2*)) 0

binAdd :: Bin -> Bin -> Bin
binAdd = zipWith (\x y -> x + y)

aggregate :: [Bin] -> Bin
aggregate = foldl binAdd (take 12 $ repeat 0)

common :: (Int -> Int -> Bool) -> [Bin] -> Bin
common f ns = map ((bool 0 1) . (f majority)) (aggregate ns)
  where
    majority = ceiling (fromIntegral (length ns) / 2)

mostCommon :: [Bin] -> Bin
mostCommon = common (>)

leastCommon :: [Bin] -> Bin
leastCommon = common (<=)

criteriaFilter :: ([Bin] -> Bin) -> [Bin] -> Bin
criteriaFilter f = helper 0
  where
    helper :: Int -> [Bin] -> Bin
    helper _  [b] = b
    helper n bins = let target = f bins in
                      helper (n+1) $ filter (\b -> b !! n == target !! n) bins

gamma :: [Bin] -> Int
gamma = binToInt . mostCommon

epsilon :: [Bin] -> Int
epsilon = binToInt . leastCommon

oxygen :: [Bin] -> Int
oxygen = binToInt . criteriaFilter mostCommon

scrubber :: [Bin] -> Int
scrubber = binToInt . criteriaFilter leastCommon

solve2 :: String -> String
solve2 str = show $ oxygen input * scrubber input
  where
    input = map (map digitToInt) (lines str)
