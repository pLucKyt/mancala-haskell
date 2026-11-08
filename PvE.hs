module PvE where

-- since PvP and PvE both have main function(for future use if ever used), we gotta manually import since we would have ambiguity with PvP's main function
import PvP (readInput, realIndex, isCapture)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Mancala
import Minimax

-- depth 5 is instant
-- depth 6 takes about 3-6s, with t ~ instant as the game progresses and the tree becomes smaller
botDifficult :: Int
botDifficult = 6

readRnd :: String -> String
readRnd "Player" = "Player"
readRnd "player" = "Player"
readRnd "Bot" = "Bot"
readRnd "bot" = "Bot"
readRnd _ = "Random"

createStart :: String -> Int -> GameState
createStart "Player" _ = mancalaInitialState
createStart "Bot" _ = GameState (board mancalaInitialState) (opponent (onTurn mancalaInitialState))
createStart _ b = if b == 0 then mancalaInitialState else GameState (board mancalaInitialState) (opponent (onTurn mancalaInitialState))

-- the Player is always PlayerA(the one on the bottom) and the Bot is always PlayerB(the one on the top)
renamePlayer :: Player -> String
renamePlayer PlayerA = "Player"
renamePlayer PlayerB = "Bot"

funnyFunction :: Int -> GameState -> IO ()
funnyFunction k gs = 
                    do
                    let newGs = playMove k gs                                        
                    if onTurn gs == onTurn newGs
                        then
                            playPvE (TurnLogic (renamePlayer (onTurn newGs)) True) newGs
                        else
                            if isCapture k gs
                                then
                                    playPvE (CaptureSignal (renamePlayer (onTurn gs))) newGs
                                else
                                    playPvE (TurnLogic (renamePlayer (onTurn newGs)) False) newGs

{- HLINT ignore "Use notElem" -}
playPvE :: MessageInfo -> GameState -> IO ()
playPvE msg gs = 
                if isGameOver gs
                    then
                        putStrLn (drawBoard (Outcome (fmap renamePlayer (whoWins gs))) gs)
                    else
                        do
                        putStrLn (drawBoard msg gs)
                        if onTurn gs == PlayerA
                            then
                                do
                                line <- getLine
                                let n = readInput line
                                let k = realIndex n gs
                                if n == -1
                                    then                            
                                        playPvE (ForbiddenPlay n) gs
                                    else
                                        if not (k `elem` validMoves gs)
                                            then
                                                playPvE (ForbiddenPlay k) gs                              
                                            else
                                                funnyFunction k gs
                            else
                                do                                
                                let Just k = bestMove botDifficult gs -- would crash if it's Nothing, but it's impossible, because we catch that above basically
                                funnyFunction k gs


main :: IO ()
main = do
    putStrLn "Choose who plays first: Player, Bot or Random"
    line <- getLine
    t <- getPOSIXTime
    let lineF = readRnd line
    let rndBit = floor t `mod` 2
    let modStart = createStart lineF rndBit
    putStrLn "Game On!!"
    playPvE (TurnLogic (renamePlayer (onTurn modStart)) False) modStart


