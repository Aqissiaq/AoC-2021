
main :: IO ()
main = interact solve2

solve1 :: String -> String
solve1 str = show . length $ foldl (\fish _ -> oneDay fish) input [1..256]
  where
    input = read $ "["++str++"]"

oneDay :: [Int] -> [Int]
oneDay [] = []
oneDay (0:rest) = [6,8] ++ oneDay rest
oneDay (n:rest) = (n-1) : oneDay rest

type FishList = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

createFishList :: [Int] -> FishList
createFishList ns = (count 0 ns, count 1 ns, count 2 ns, count 3 ns, count 4 ns, count 5 ns, count 6 ns, count 7 ns, count 8 ns)

rotate :: FishList -> FishList
rotate (a,b,c,d,e,f,g,h,i) = (b, c, d, e, f, g, h+a, i, a)

countFish :: FishList -> Int
countFish (a,b,c,d,e,f,g,h,i) = sum [a,b,c,d,e,f,g,h,i]

solve2 :: String -> String
solve2 str = show . countFish $ foldl (\fish _ -> rotate fish) input [1..256]
  where
    input = createFishList $ read $ "["++str++"]"
