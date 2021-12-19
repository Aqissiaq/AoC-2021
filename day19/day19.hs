import Data.List.Split
import Data.Maybe
import Data.Either
import qualified Data.Set as S
import qualified Data.Map as M

type Point = (Int, Int, Int)
type Offset = (Int, Int, Int)
data Scanner = Scanner { sid :: Int, beacons :: (S.Set Point)}
  deriving (Ord, Eq)

instance Show Scanner where
  show = ("Scanner: "++) . show . sid

main :: IO ()
main = do input <- getContents
          let scanners = parse input
              located = locateAll (M.fromList [((head scanners), (0,0,0))]) (tail scanners)
              positions = M.elems located
          -- part 1: how many beacons?
          print $ S.size $ translateAll located
          -- part 2: longest distance between scanners
          print $ maximum [manhattan p q | p <- positions, q <- positions]

parse :: String -> [Scanner]
parse = map parseScanner . zip [0..] . splitOn [""] . lines
  where
    parseBeacons = S.fromList . map (\s -> read $ "("++s++")")
    parseScanner (i, bs) = Scanner i (parseBeacons $ tail bs)

rotateX,rotateY,rotateZ :: S.Set Point -> S.Set Point
rotateX = S.map (\(x,y,z) -> (x,-z,y))
rotateY = S.map (\(x,y,z) -> (-z,y,x))
rotateZ = S.map (\(x,y,z) -> (-y,x,z))

allRotations :: S.Set Point -> S.Set (S.Set Point)
allRotations s = S.fromList [iterate rotateZ (iterate rotateY (iterate rotateX s !! x) !! y) !! z
                            | x <- [0..3], y <- [0..3], z <- [0..3]]

translateBy :: Point -> S.Set Point -> S.Set Point
translateBy (dx,dy,dz) = S.map (\(x,y,z) -> (x+dx, y+dy, z+dz))

translateAll :: M.Map Scanner Offset -> S.Set Point
translateAll m = S.unions [(translateBy off) (beacons s) | (s, off) <- M.toList m]

sub, add :: Point -> Point -> Offset
sub (x0,y0,z0) (x1,y1,z1) = (x0-x1, y0-y1, z0-z1)
add (x0,y0,z0) (x1,y1,z1) = (x0+x1, y0+y1, z0+z1)

manhattan :: Point -> Point -> Int
manhattan (x0,y0,z0) (x1,y1,z1) = abs (x0-x1 + y0-y1 + z0-z1)

offsetCounts :: S.Set Point -> S.Set Point -> M.Map Offset Int
offsetCounts s1 s2 = M.fromListWith (+) [(x `sub` y, 1) | x <- S.toList s1, y <- S.toList s2]

-- this is an absolute disaster, but it works ¯\_(ツ)_/¯
locateAll :: M.Map Scanner Offset -> [Scanner] -> M.Map Scanner Offset
locateAll located [] = located
locateAll located scanners = let (newLoc, leftover) = partitionEithers $ map lockin scanners in
                               locateAll (located `M.union` M.fromList newLoc) leftover
  where
    lockin :: Scanner -> Either (Scanner, Offset) Scanner
    lockin new = let matches = filter (isJust . snd) $ map (\old -> (old, locateRelative old new)) (M.keys located)
                     oldOffs = map ((located M.!) . fst) matches
                  in if null matches then Right new else
                       let  (newScan, newOff) = fromJust $ snd (head matches)
                            oldOff = head oldOffs in
                         Left (newScan, newOff `add` oldOff)

locateRelative :: Scanner -> Scanner -> Maybe (Scanner, Offset)
locateRelative (Scanner _ b1) (Scanner id2 b2) = S.findMax $ S.map overlaps offsets
  where
    b2Rots = allRotations b2
    offsets = S.map (\s -> (offsetCounts b1 s, s)) b2Rots
    overlaps (m, s) = if M.null $ M.filter (>11) m then Nothing else let (offs, _) = M.findMax $ M.filter (>11) m in
      Just $ (Scanner id2 s, offs)
