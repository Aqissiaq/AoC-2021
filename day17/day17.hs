import Data.List.Split
import Data.Maybe

main :: IO ()
main = do input <- getContents
          let target = parseInput input
          -- brute force part 1...
          print $ highestVelocity 250 250 target
          -- ...and part 2
          print $ countDistinct 250 250 target

type Point = (Int, Int)
type Velocity = (Int, Int)
type Target = (Int, Int, Int, Int)

parseInput :: String -> Target
parseInput str = let [_, xs, _, ys] = splitOneOf [',', '='] str
                     [x1, x2] = map read $ splitOn ".." xs
                     [y1,y2] = map read $ splitOn ".." ys in
                   (x1, x2, y1,y2)

step :: (Point, Velocity) -> (Point, Velocity)
step ((x,y), (vx,vy)) = ((x + vx, y + vy), (max (vx-1) 0, vy-1))

inTarget :: Target -> Point -> Bool
inTarget (tx1, tx2, ty1, ty2) (x,y) = and [x >= tx1, x <= tx2, y >= ty1, y <= ty2]

-- maybe maximum height reached (Nothing if we miss target)
heightReached :: Velocity -> Target -> Maybe Int
heightReached v t@(_, tx2, ty1, _) = if any (inTarget t) trajectory
                                   then Just maxHeight else Nothing
  where
    overshot (x,y) = y < ty1 || x > tx2
    trajectory = takeWhile (not . overshot) $ map fst $ iterate step ((0,0),v)
    maxHeight = maximum $ map snd trajectory

highestVelocity :: Int -> Int -> Target -> Int
highestVelocity maxX maxY t = maximum $ catMaybes maxHeights
  where
    maxHeights = [heightReached v t | v <- [(x,y) | x <- [0..maxX], y <- [0..maxY]]]

countDistinct :: Int -> Int -> Target -> Int
countDistinct maxX maxY t = length $ catMaybes hits
  where
    hits = [heightReached v t | v <- [(x,y) | x <- [0..maxX], y <- [-maxY..maxY]]]
