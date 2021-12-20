import Data.List.Split
import Data.Either
import Data.Maybe
import qualified Data.Set as S

import Debug.Trace

type Algo = [Char]
type Point = (Int, Int)
type Image = S.Set Point

binToDec :: [Int] -> Int
binToDec = foldl ((+) . (2*)) 0

parseImage :: [String] -> Image
parseImage str = S.fromList [(x, y) | (y, row) <- zip [0..] str, (x, c) <- zip [0..] row, c == '#']

ppImage :: Image -> IO ()
ppImage img = putStr $ unlines [[show' (x,y) | x <- [minX..maxX]] | y <- [minY..maxY]]
  where
    show' (x,y) = if (x,y) `S.member` img then '#' else '.'
    maxX = S.findMax $ S.map fst img
    maxY = S.findMax $ S.map snd img
    minX = S.findMin $ S.map fst img
    minY = S.findMin $ S.map snd img

main :: IO ()
main = do input <- getContents
          let [algo, rawImage] = splitOn [""] (lines input)
              image = parseImage rawImage
              algorithm = unlines algo
          -- part 1
          print . S.size $ stepN 2 algorithm True image
          -- part 2
          print . S.size $ stepN 50 algorithm True image

getNeighborhood :: Point -> [Point]
getNeighborhood (x,y) = [(x+i, y+j) | j <- [-1..1], i <- [-1..1]]


stepN :: Int -> Algo -> Bool -> Image -> Image
stepN 0 _ _ img = img
stepN n alg outsideOn img = step alg outsideOn $ stepN (n-1) alg (not outsideOn) img

step :: Algo -> Bool -> Image -> Image
step algo outsideOn img = S.fromList $ mapMaybe foo boundingBox
  where
    maxX = (S.findMax $ S.map fst img) + 1
    maxY = (S.findMax $ S.map snd img) + 1
    minX = (S.findMin $ S.map fst img) - 1
    minY = (S.findMin $ S.map snd img) - 1
    boundingBox = [(x,y) | x <- [minX..maxX], y <- [minY..maxY]]
    isOn (x,y) | outsideOn && (x <= minX || x >= maxX || y <= minY || y >= maxY) = True
               | (x,y) `S.member` img = True
               | otherwise = False
    pixelToInt p = binToDec [if isOn x then 1 else 0 | x <- getNeighborhood p]
    foo :: Point -> Maybe Point
    foo p | algo !! (pixelToInt p) == '#' = Just p
          | otherwise = Nothing
