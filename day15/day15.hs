import qualified Data.Map as M
import Data.Char
import Data.Maybe

-- https://stackoverflow.com/questions/14012603/how-to-implement-dijkstra-algorithm-in-haskell
import qualified Data.Set as Set

spfa :: (Ord cost , Ord node)
    => ((cost , node) -> [(cost , node)]) -- ^ Where we can go from a node and the cost of that
    -> node                               -- ^ Where we want to get to
    -> (cost , node)                      -- ^ The start position
    -> Maybe (cost , node)                -- ^ Maybe the answer. Maybe it doesn't exist
spfa next target start = search mempty (Set.singleton start)
    where
        search visited toBeVisited = case Set.minView toBeVisited of
            Nothing -> Nothing
            Just ((cost , vertex) , withoutVertex)
                | vertex == target            -> Just (cost , vertex)
                | vertex `Set.member` visited -> search visited withoutVertex
                | otherwise                   -> search visitedWithNode withNext
                where
                    visitedWithNode = Set.insert vertex visited
                    withNext = foldr Set.insert withoutVertex $ next (cost , vertex)

type Coord = (Int, Int)
type Cavern = M.Map Coord Int

main :: IO ()
main = do input <- getContents
          let cavern = parseCavern input
              part2 = part2Cavern cavern
          print $ spfa (costFunc cavern) (fst $ M.findMax cavern) (0, (1,1))
          print $ spfa (costFunc part2) (fst $ M.findMax part2) (0, (1,1))

parseCavern :: String -> Cavern
parseCavern str = M.fromList [((x,y), digitToInt danger) |
                               (row, y) <- zip (lines str) [1..],
                               (danger, x) <- zip row [1..]]

part2Cavern :: Cavern -> Cavern
part2Cavern originalCavern = let
  (originalW, originalH) = fst $ M.findMax originalCavern
  newCaverns = [M.fromList [((x + (tileX * originalW), y + (tileY * originalH)), wrapAround (danger + tileX + tileY))
                           | tileX <- [0..4], tileY <- [0..4]]
               | ((x,y),danger) <- M.toList originalCavern]
  in M.unions $ newCaverns

wrapAround :: Int -> Int
wrapAround n | n > 9 = (n `mod` 10)+1
             | otherwise = n

getNeighbors :: Coord -> [Coord]
getNeighbors (x,y) = [(x+i, y+j) | i <- [-1..1], j <- [-1..1], i+j /= 0, i+j < 2]

costFunc :: Cavern -> (Int, Coord) -> [(Int, Coord)]
costFunc cavern (cost,c) = map(\p ->((fromMaybe 10 $ M.lookup p cavern) + cost, p)) $ getNeighbors c
