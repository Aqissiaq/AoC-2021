type Roll = (Int, Int, Int)
data Player = Player {pos :: Int, score :: Int}
  deriving Show
data GameState = GS { p1 :: Player,
                      p2 :: Player,
                      p1Up :: Bool,
                      dieRolls :: Int}
  deriving Show

example, input :: GameState
example = GS (Player 4 0) (Player 8 0) True 0
input = GS (Player 8 0) (Player 3 0) True 0

oneTurn :: Roll -> GameState -> GameState
oneTurn (d1, d2, d3) (GS (Player p1Pos p1Score) p2 True rolls) = let newPos = wrapAround $ p1Pos + d1 + d2 + d3
                                                                     newScore = p1Score + newPos
  in GS (Player newPos newScore) p2 False (rolls + 3)
oneTurn (d1, d2, d3) (GS p1 (Player p2Pos p2Score) False rolls) = let newPos = wrapAround $ p2Pos + d1 + d2 + d3
                                                                      newScore = p2Score + newPos
  in GS p1 (Player newPos newScore) True (rolls + 3)

wrapAround :: Int -> Int
wrapAround n | n > 9 = (n `mod` 10) + 1
             | otherwise = n

play :: Roll -> GameState -> Int
play (d1,d2,d3) gs | score (p1 gs) >= 1000 = score (p2 gs) * dieRolls gs
                   | score (p2 gs) >= 1000 = score (p1 gs) * dieRolls gs
                   | otherwise = let newRoll = (d3+1, d3+2, d3+3) in play newRoll $ oneTurn newRoll gs
