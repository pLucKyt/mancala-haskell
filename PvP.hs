module PvP where

import Mancala

readInput :: String -> Int
readInput "1" = 1
readInput "2" = 2
readInput "3" = 3
readInput "4" = 4
readInput "5" = 5
readInput "6" = 6
readInput _ = -1

realIndex :: Int -> GameState -> Int
realIndex n gs = if turn == PlayerA then n - 1 else n + 6
            where
                turn = onTurn gs

{- HLINT ignore "Use uncurry" -}
isCapture :: Int -> GameState -> Bool
isCapture k gs = snd sowed /= applyCapture turn (fst sowed) (snd sowed)
            where
                t = board gs
                turn = onTurn gs
                sowed = sow turn k (t !! k) (changeAt k (const 0) t)

-- MessageInfo is used to store information regarding the previous step, because each gs is first drawn on the screen, then stuff is computed
{- HLINT ignore "Use notElem" -}
playPvP :: MessageInfo -> GameState -> IO ()
playPvP msg gs = 
                if isGameOver gs
                    then
                        putStrLn (drawBoard (Outcome (fmap show (whoWins gs))) gs)
                    else
                        do
                        putStrLn (drawBoard msg gs)                    
                        line <- getLine
                        let n = readInput line
                        let k = realIndex n gs
                        if n == -1
                            then                            
                                playPvP (ForbiddenPlay n) gs
                            else
                                if not (k `elem` validMoves gs)
                                    then
                                        playPvP (ForbiddenPlay k) gs                              
                                    else    
                                        do
                                        let newGs = playMove k gs                                        
                                        if onTurn gs == onTurn newGs
                                            then
                                                playPvP (TurnLogic (show (onTurn newGs)) True) newGs
                                            else
                                                if isCapture k gs
                                                    then
                                                        playPvP (CaptureSignal (show (onTurn gs))) newGs
                                                    else
                                                        playPvP (TurnLogic (show (onTurn newGs)) False) newGs


main :: IO ()
main = do
    putStrLn "Game On!!"
    playPvP (TurnLogic (show (onTurn mancalaInitialState)) False) mancalaInitialState
    

