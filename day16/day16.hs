import Data.Char

type Hex = String
type Bin = String

data Packet = Literal Int Int
            | Operator Int Int [Packet]
  deriving Show

main :: IO ()
main = do
  input <- getContents
  let (result, _) = parse input
  print $ sumVersions result
  print $ eval result

parse = parsePacket . hexToBin

sumVersions :: Packet -> Int
sumVersions (Literal version _) = version
sumVersions (Operator version _ ps) = version + sum [sumVersions p | p <- ps]

eval :: Packet -> Int
eval (Literal _ value) = value
eval (Operator _ 0 ps) = sum [eval p | p <- ps]
eval (Operator _ 1 ps) = product [eval p | p <- ps]
eval (Operator _ 2 ps) = minimum [eval p | p <- ps]
eval (Operator _ 3 ps) = maximum [eval p | p <- ps]
eval (Operator _ 5 [p1,p2]) = if eval p1 > eval p2 then 1 else 0
eval (Operator _ 6 [p1,p2]) = if eval p1 < eval p2 then 1 else 0
eval (Operator _ 7 [p1,p2]) = if eval p1 == eval p2 then 1 else 0
eval e = error $ "Malformed package in eval: " ++ show e

parsePacket :: Bin -> (Packet, Bin)
parsePacket binStr = let version = binToDec $ chunk 0 3 binStr
                         typeID  = chunk 3 6 binStr
                         remain  = drop 6 binStr in
  case binToDec typeID of
    4 -> let (value, rest) = parseValue remain in (Literal version value, rest)
    n -> let (subPackets, rest) = parseOperator remain in (Operator version n subPackets, rest)

parseValue :: Bin -> (Int, Bin)
parseValue binStr = let binValue = buildValue binStr
                        l = length binValue in
  (binToDec binValue, drop (l + (l `div` 4)) binStr)

buildValue :: Bin -> Bin
buildValue ('1':rest) = take 4 rest ++ (buildValue $ drop 4 rest)
buildValue ('0':rest) = take 4 rest
buildValue e = error $ "cannot build value from " ++ show e

parseOperator :: Bin -> ([Packet], Bin)
parseOperator ('0':rest) = let l = binToDec $ take 15 $ rest in parsePacketsL l (drop 15 rest)
parseOperator ('1':rest) = let n = binToDec $ take 11 $ rest in parsePacketsN n (drop 11 rest)
parseOperator e = error $ "cannot parse packet list: " ++ show e

parsePacketsL :: Int -> Bin -> ([Packet], Bin)
parsePacketsL 0 binStr = ([], binStr)
parsePacketsL l binStr = let (p, rest) = parsePacket binStr
                             originalL = length binStr
                             restL = length rest
                             parsedL = originalL - restL
                             (ackets, rests) = parsePacketsL (l - parsedL) $ drop parsedL binStr in
                           (p:ackets, rests)

parsePacketsN :: Int -> Bin -> ([Packet], Bin)
parsePacketsN 0 binStr = ([], binStr)
parsePacketsN n binStr = let (p, rest) = parsePacket binStr
                             (ackets, rests) = parsePacketsN (n-1) rest in
                           (p:ackets, rests)

chunk :: Int -> Int -> [a] -> [a]
chunk start end = drop start . take end

hexToBin :: Hex -> Bin
hexToBin = concatMap translate
  where
    translate '0' = "0000"
    translate '1' = "0001"
    translate '2' = "0010"
    translate '3' = "0011"
    translate '4' = "0100"
    translate '5' = "0101"
    translate '6' = "0110"
    translate '7' = "0111"
    translate '8' = "1000"
    translate '9' = "1001"
    translate 'A' = "1010"
    translate 'B' = "1011"
    translate 'C' = "1100"
    translate 'D' = "1101"
    translate 'E' = "1110"
    translate 'F' = "1111"
    translate _ = ""

binToDec :: Bin -> Int
binToDec = foldl ((+) . (2*)) 0 . map digitToInt

