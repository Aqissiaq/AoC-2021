{-# LANGUAGE TupleSections #-}
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Control.Applicative

main :: IO ()
main = interact solve

solve :: String -> String
solve str = show part2
  where
    steps = map parseStep $ lines str
    part1 = S.size $ foldl' step S.empty $ filter part1Valid steps
    part2Reactor = foldl' step2 [] steps
    part2 = sum . map (\(r,s) -> volume r * s) $ part2Reactor

type Interval = (Int, Int)
type Cube = (Interval, Interval, Interval)
data Step = On Cube | Off Cube
  deriving Show
type Reactor = S.Set (Int, Int, Int)

parseStep :: String -> Step
parseStep str = case words str of
  ["on", cube] -> On $ parseCube cube
  ["off", cube] -> Off $ parseCube cube

parseCube :: String -> Cube
parseCube str = let [x,y,z] = splitOn "," str
  in (parseInterval x, parseInterval y, parseInterval z)

parseInterval :: String -> Interval
parseInterval str = let [_, rest] = splitOn "=" str
                        [start, end] = sort . map read $ splitOn ".." rest in
                   (start, end)

part1Valid :: Step -> Bool
part1Valid (On ((x0,x1), (y0,y1), (z0,z1))) = all (liftA2 (&&) (>= -50) (<= 50)) [x0,x1,y0,y1,z0,z1]
part1Valid (Off ((x0,x1), (y0,y1), (z0,z1))) = all (liftA2 (&&) (>= -50) (<= 50)) [x0,x1,y0,y1,z0,z1]

step :: Reactor -> Step -> Reactor
step r (On ((x0,x1), (y0,y1), (z0,z1))) = r `S.union`
                                        (S.fromList [(x,y,z) | x <- [x0..x1], y <- [y0..y1], z <- [z0..z1]])
step r (Off ((x0,x1), (y0,y1), (z0,z1))) = r `S.difference`
                                        (S.fromList [(x,y,z) | x <- [x0..x1], y <- [y0..y1], z <- [z0..z1]])

len :: Interval -> Int
len (a,b) = b - a + 1

volume :: Cube -> Int
volume (r1, r2, r3) = (len r1) * (len r2) * (len r3)

intersectI :: Interval -> Interval -> Maybe Interval
intersectI (startA,endA) (startB,endB)
  | startB > endA || startA > endB = Nothing
  | otherwise = Just (max startA startB, min endA endB)

intersectC :: Cube -> Cube -> Maybe Cube
intersectC (rx0, ry0, rz0) (rx1, ry1, rz1) = do
  xInt <- intersectI rx0 rx1
  yInt <- intersectI ry0 ry1
  zInt <- intersectI rz0 rz1
  pure (xInt, yInt, zInt)
  -- monads!

type Reactor2 = [(Cube, Int)]

add,sub :: Reactor2 -> Cube -> Reactor2
add cubes new = (new, 1) : (cubes `sub` new)

sub cubes new = cubes ++ ints cubes
  where
    ints = mapMaybe (\(cube, count)
                     -> do int <- new `intersectC` cube
                           pure (int, -count))

step2 :: Reactor2 -> Step -> Reactor2
step2 r (On cube) = r `add` cube
step2 r (Off cube) = r `sub` cube
