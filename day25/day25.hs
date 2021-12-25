import qualified Data.Set as S
import Data.Either

type Seafloor = (S.Set (Int, Int), S.Set (Int, Int), (Int, Int))

-- pretty print for debugging
ppSeafloor :: Seafloor -> String
ppSeafloor (easts, souths, (maxX, maxY)) = unlines
  [[showCucumber (x,y) | x <- [0..maxX]] | y <- [0..maxY]]
  where
    showCucumber p | p `S.member` easts = '>'
                   | p `S.member` souths = 'v'
                   | otherwise = '.'

parseSeafloor :: [String] -> Seafloor
parseSeafloor str = let (easts, souths) =
                       partitionEithers [if c == '>' then Left (x,y) else Right (x,y)
                                        | (y, row) <- zip [0..] str, (x, c) <- zip [0..] row, c /= '.']
                     maxY = length str
                     maxX = length (head str)
                 in
                   (S.fromList easts, S.fromList souths, (maxX, maxY))

checkEast, checkSouth :: (Int, Int) -> Seafloor -> Bool
checkEast (x,y) (easts, souths, (maxX, _)) =
  let east = ((x+1) `mod` maxX, y) in not (east `S.member` easts) && not (east `S.member` souths)
checkSouth (x,y) (easts, souths, (_, maxY)) =
  let south = (x, (y+1) `mod` maxY) in not (south `S.member` easts) && not (south `S.member` souths)

moveEast, moveSouth :: Seafloor -> (Int, Int) -> (Int, Int)
moveEast s@(_, _, (maxX, _)) c@(x,y)= if checkEast c s then ((x+1) `mod` maxX, y) else c
moveSouth s@(_, _, (_, maxY)) c@(x,y)= if checkSouth c s then (x, (y+1) `mod` maxY) else c

step :: Seafloor -> Seafloor
step s@(easts, souths, bounds) = (updatedEasts, updatedSouths, bounds)
  where
    updatedEasts = S.map (moveEast s) easts
    updatedSeafloor = (updatedEasts, souths, bounds)
    updatedSouths = S.map (moveSouth updatedSeafloor) souths

solve :: Seafloor -> Int -> Int
solve s n | s == step s = n
          | otherwise = solve (step s) (n+1)

main :: IO ()
main = do input <- getContents
          let seafloor = parseSeafloor (lines input)
          print $ solve seafloor 1
