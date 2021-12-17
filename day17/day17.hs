import Data.List.Split
import Data.Maybe

main :: IO ()
main = do input <- getContents
          let target = parseInput input
          -- part 1
          print $ testVelocities 250 250 target
          -- part 2
          print $ part2 250 250 target

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

reachesTarget :: Velocity -> Target -> Maybe Int
reachesTarget v t@(_, _, ty1, _) = if any (inTarget t) $ trajHead
                                   then Just maxHeight else Nothing
  where
    trajectory = map fst $ iterate step ((0,0),v)
    highEnough (_,y) = y >= ty1
    trajHead = takeWhile highEnough trajectory
    maxHeight = maximum $ map snd trajHead

testVelocities :: Int -> Int -> Target -> Int
testVelocities maxX maxY t = maximum $ catMaybes maxHeights
  where
    maxHeights = [reachesTarget v t | v <- [(x,y) | x <- [0..maxX], y <- [0..maxY]]]

part2 :: Int -> Int -> Target -> Int
part2 maxX maxY t = length $ catMaybes trajs
  where
    trajs = [reachesTarget v t | v <- [(x,y) | x <- [0..maxX], y <- [-maxY..maxY]]]
