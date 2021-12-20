import Data.List.Split
import Data.Maybe
import Data.Bool
import qualified Data.Set as S

type Algo = [Char]
type Point = (Int, Int)
type Image = S.Set Point

binToDec :: [Int] -> Int
binToDec = foldl ((+) . (2*)) 0

parseImage :: [String] -> Image
parseImage str = S.fromList [(x, y) | (y, row) <- zip [0..] str, (x, c) <- zip [0..] row, c == '#']

main :: IO ()
main = do input <- getContents
          let [algo, rawImage] = splitOn [""] (lines input)
              image = parseImage rawImage
              algorithm = unlines algo
          -- part 1
          print . S.size $ enhanceN 2 algorithm image
          -- part 2
          print . S.size $ enhanceN 50 algorithm image

getNeighborhood :: Point -> [Point]
getNeighborhood (x,y) = [(x+i, y+j) | j <- [-1..1], i <- [-1..1]]

enhanceN :: Int -> Algo -> Image -> Image
enhanceN n alg img | head alg == '.' = iterate (enhance alg False) img !! n
                | otherwise = enhanceN' n True
  where
    enhanceN' :: Int -> Bool -> Image
    enhanceN' 0 _ = img
    enhanceN' i outsideOn = enhance alg outsideOn $ enhanceN' (i-1) (not outsideOn)

enhance :: Algo -> Bool -> Image -> Image
enhance algo outsideOn img = S.fromList $ mapMaybe conv boundingBox
  where
    maxX = (S.findMax $ S.map fst img) + 1
    maxY = (S.findMax $ S.map snd img) + 1
    minX = (S.findMin $ S.map fst img) - 1
    minY = (S.findMin $ S.map snd img) - 1
    boundingBox = [(x,y) | x <- [minX..maxX], y <- [minY..maxY]]
    outOfBounds (x,y) = x <= minX || x >= maxX || y <= minY || y >= maxY
    isOn p = (outsideOn && outOfBounds p) || p `S.member` img
    pixelToInt p = binToDec [bool 0 1 (isOn q) | q <- getNeighborhood p]
    conv p | algo !! (pixelToInt p) == '#' = Just p
           | otherwise = Nothing
