import Control.Monad
import Data.Array
import Data.Monoid

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
-- wrapAround n | n > 9 = (n `mod` 10) + 1
-- this works for part 1, for some reason???
-- somehow cancels out forgetting to wrap the dice values¿¿
wrapAround n | n > 10 = wrapAround $ n - 10
             | otherwise = n

play :: Roll -> GameState -> Int
play (d1,d2,d3) gs | score (p1 gs) >= 1000 = score (p2 gs) * dieRolls gs
                   | score (p2 gs) >= 1000 = score (p1 gs) * dieRolls gs
                   | otherwise = let newRoll = (d1+3, d2+3, d3+3)
                     in play newRoll $ oneTurn newRoll gs

solve2 :: Int -> Int -> Sum Int
solve2 p1Start p2Start = uncurry max $ dp ! ((0, p1Start), (0, p2Start), True)
  where
    limits = (((0,1), (0,1), False), ((30, 10), (30, 10), True))
    dp = listArray limits [countWins p | p <- range limits]
    countWins :: ((Int, Int), (Int, Int), Bool) -> (Sum Int, Sum Int)
    countWins ((p1Score, p1Pos), (p2Score, p2Pos), p1Active)
      | p1Score >= 21 = (1,0)
      | p2Score >= 21 = (0,1)
      | p1Active      = mconcat [dp ! ((p1Score + p, p), (p2Score, p2Pos), False)
                                | p <- positions p1Pos]
      | otherwise     = mconcat [dp ! ((p1Score , p1Pos), (p2Score + p, p), True)
                                | p <- positions p2Pos]
    positions current = [wrapAround (current + roll) | roll <- rolls]
    rolls = map sum $ replicateM 3 [1,2,3]

main :: IO ()
main = do putStr $ "Part 1: "
          print $ play (1,2,3) input
          putStr $ "Part 2: "
          print . getSum $ solve2 8 3
