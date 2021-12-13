import qualified Data.Set as S
import Data.List.Split

data Fold = X Int | Y Int deriving(Show)
type Point = (Int, Int)
type Transp = S.Set Point

parseTransparent :: String -> Transp
parseTransparent str = S.fromList [read $ "("++line++")" | line <- lines str]

parseFold :: String -> Fold
parseFold str = case splitOn "=" $ (!! 2) $ words str of
                  ["x", n] -> X $ read n
                  ["y", n] -> Y $ read n
                  _ -> error "Malformed fold input"

fold :: Fold -> Transp -> Transp
fold (X n) trsp = (mirror left) `S.union` right
  where
    (left, right) = S.partition (\(x,_) -> x > n) trsp
    mirror = S.map (\(x,y) -> (((x - n) * (-1)) + n,y))
fold (Y n) trsp = (mirror top) `S.union` bot
  where
    (top, bot) = S.partition (\(_,y) -> y > n) trsp
    mirror = S.map (\(x,y) -> (x, ((y - n) * (-1)) + n))

ppTransp :: Transp -> String
ppTransp trsp = unlines
  [[if (x,y) `S.member` trsp then '#' else ' ' | x <- [0..maxX]] | y <- [0..maxY]]
  where
    maxX = S.findMax $ S.map fst trsp
    maxY = S.findMax $ S.map snd trsp

main :: IO ()
main = do input <- getContents
          let [t, f] = splitOn "\n\n" input
              transp = parseTransparent t
              folds = map parseFold (lines f)
          -- part 1
          print . S.size $ fold (head folds) transp
          -- part 2
          putStrLn . ppTransp $ foldl (flip fold) transp folds
