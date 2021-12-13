import qualified Data.Map as M
import Data.List.Split
import Data.Char

type Node = String
type Graph = M.Map Node [Node]

countPaths :: Graph -> Node -> Node -> [Node] -> Bool -> Int
countPaths graph u t visited allowRevisits
  | u == t           = 1
  | u `elem` visited = if allowRevisits && u /= "start"
    then sum [countPaths graph c t visited False | c <- graph M.! u] else 0
  | all isUpper u    = sum [countPaths graph c t visited allowRevisits | c <- graph M.! u]
  | otherwise        = sum [countPaths graph c t (u:visited) allowRevisits | c <- graph M.! u]

makeGraph :: String -> Graph
makeGraph str = M.fromListWith (++) $ concatMap makeEdge (lines str)
  where makeEdge line = let [src, dest] = splitOn "-" line in
                          [(src, [dest]), (dest, [src])]

-- just hiding some "default" args
solve :: Graph -> Bool -> Int
solve graph = countPaths graph "start" "end" []

main :: IO ()
main = do input <- getContents
          let inputG = makeGraph input
          print $ solve inputG False
          print $ solve inputG True
