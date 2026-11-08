module Minimax where

import Rose
import Mancala

-- difference in stones in the stores
fS :: [Int] -> Int
fS t = t !! storeA - t !! storeB

-- the current state of the game
-- the whole point of this function is to calculate the difference in the store of player on turn for each possible valid move, then take the maximum of those values
{- HLINT ignore "Use :" -}
fC :: GameState -> Int
fC gs = maximum ([0] ++ [board (playMove i gs) !! trueStore - t !! trueStore | i <- validMoves gs])
    where        
        t = board gs        
        trueStore = myStore (onTurn gs)

-- starting gamestate(with corresponding player setup)
-- the player at the start is preserved inside gs
freeTurnMoves :: GameState -> Int
freeTurnMoves gs = foldl (\acc x -> if onTurn gs == onTurn x then acc + 1 else acc) 0 [playMove i gs | i <- validMoves gs]

-- the current state of the game
-- the whole point of this function is to calculate the difference in exact order, PlayerA - PlayerB, for the free turns that are possible to have from the current state
-- since information regarding both parties is useful in our heuristic, we force the playMove call inside freeTurnMoves by artificially setting each player to be on turn, when in reality one isn't
fF :: GameState -> Int
fF gs = freeTurnMoves (GameState t PlayerA) - freeTurnMoves (GameState t PlayerB)
    where
        t = board gs

-- the observed player, kept for recursive calls -> the state in which we are computing the heuristic
-- fS and fF both compute for each player, so fC needs the adjustment in sign because it only computes for the player in turn, the whole formula is in this way antisymmetric
heuristic :: Player -> GameState -> Int
heuristic p gs = (if p == PlayerA then 1 else -1) * (6 * fS t + (if turn == PlayerA then 1 else -1) * 3 * fC gs + 5 * fF gs)
            where
                t = board gs
                turn = onTurn gs

-- the player is carried through the whole recursion, because that's the information for what we are playing for, meaning who the Me is
minimax :: Player -> Rose GameState -> Int
minimax p (Node gs []) = heuristic p gs
minimax p (Node gs xs) = if turn == p then maximum v else minimum v
                    where
                        turn = onTurn gs
                        v = [minimax p x | x <- xs]

-- gives us the index of the pit/move that will lead us into a state where we have the "biggest chance"(maximum heuristic value) of winning relative to other available moves
bestMove :: Int -> GameState -> Maybe Int
bestMove d gs = if null heurIndexOfNextMove then Nothing else Just (snd (maximum heurIndexOfNextMove)) -- maximum orders pairs by first element
            where
                Node val xs = gameTree d gs
                turn = onTurn gs
                heurIndexOfNextMove = [(minimax turn x, i) | (x, i) <- zip xs (validMoves gs)] -- since validMoves and children are in same order, we can confidently zip them
