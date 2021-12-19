import Data.List.Split
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M

type Point = (Int, Int, Int)
type Offset = (Int, Int, Int)
data Scanner = Scanner { sid :: Int, beacons :: (S.Set Point)}
  deriving Ord

instance Show Scanner where
  show = ("Scanner: "++) . show . sid
instance Eq Scanner where
  Scanner id0 _ == Scanner id1 _ = id0 == id1

ppMap :: (Show a, Show b) => M.Map a b -> String
ppMap m = unlines [(show k)++" : "++ (show v) | (k,v) <- M.toList m]

main :: IO ()
main = do input <- getContents
          let scanners = parse input
              relOffs = M.fromList $ zip [0..] $ map (relativeOffsets scanners) scanners
          putStr . ppMap $ relOffs
          print ""
          print $ combineOffsets relOffs
          -- 1. combine offsets
          -- 2. translate scanners
          -- 3. size of set

parse :: String -> [Scanner]
parse = map parseScanner . zip [0..] . splitOn [""] . lines
  where
    parseBeacons = S.fromList . map (\s -> read $ "("++s++")")
    parseScanner (i, bs) = Scanner i (parseBeacons $ tail bs)

rotateX,rotateY,rotateZ :: S.Set Point -> S.Set Point
rotateX = S.map (\(x,y,z) -> (x,z,-y))
rotateY = S.map (\(x,y,z) -> (-z,y,x))
rotateZ = S.map (\(x,y,z) -> (-y,x,z))

allRotations :: S.Set Point -> S.Set (S.Set Point)
allRotations s = S.fromList [iterate rotateZ (iterate rotateY (iterate rotateX s !! x) !! y) !! z
                            | x <- [0..3], y <- [0..3], z <- [0..3]]

translateBy :: Point -> S.Set Point -> S.Set Point
translateBy (dx,dy,dz) = S.map (\(x,y,z) -> (x+dx, y+dy, z+dz))

offset :: Point -> Point -> Offset
offset (x0,y0,z0) (x1,y1,z1) = (x1-x0, y1-y0, z1-z0)

(+++) :: Point -> Point -> Point
(x,y,z) +++ (x',y',z') = (x+x', y+y', z+z')

offsetCounts :: S.Set Point -> S.Set Point -> M.Map Offset Int
offsetCounts s1 s2 = M.fromListWith (+) [(offset x y, 1) | x <- S.toList s1, y <- S.toList s2]

locateRelative :: Scanner -> Scanner -> Maybe Offset
locateRelative (Scanner _ b1) (Scanner _ b2) = S.lookupMax valid >>= Just . fst . M.findMax
  where
    b2Rots = allRotations b2
    offsets = S.map (offsetCounts b1) b2Rots
    valid = S.filter (not . M.null) $ S.map (M.filter (>11)) offsets

relativeOffsets :: [Scanner] -> Scanner -> M.Map Int Offset
relativeOffsets scanners s = M.filter (/=(0,0,0)) . M.fromList $ [(sid, fromJust offs) | (sid, offs) <- offsets, isJust offs]
  where
    offsets = [(sid s', locateRelative s s') | s' <- scanners]

-- combineOffsets :: M.Map Int (M.Map Int Offset) -> M.Map Int Offset
combineOffsets m = combine' 0 3
  where
    combine' :: Int -> Int -> Offset
    combine' source target | M.member target (m M.! source) = (m M.! source) M.! target
                           | otherwise = let (next, offset) = M.findMax (m M.! source) in
                               offset +++ combine' next target
