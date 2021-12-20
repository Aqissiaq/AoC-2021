
main :: IO ()
main = interact solve

solve :: String -> String
solve str = show . minimum $ checkAll positions
  where
    positions = read $ "["++str++"]"

checkAll :: [Int] -> [Int]
checkAll poss = map (checkOne poss) [minimum poss .. maximum poss]
  where
    checkOne :: [Int] -> Int -> Int
    checkOne ps target = sum $ map (\x -> fuelCost2 (abs (x - target))) ps

fuelCost1 = id

fuelCost2 n = (n * (n+1)) `div` 2
