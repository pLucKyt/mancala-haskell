module Mancala where

import Draw
import Rose

data Player = PlayerA | PlayerB deriving (Eq, Show)

opponent :: Player -> Player
opponent PlayerA = PlayerB
opponent PlayerB = PlayerA

data GameState = GameState {board :: [Int], onTurn :: Player} deriving Eq

storeA :: Int
storeA = 6

storeB :: Int
storeB = 13

pitsA :: [Int]
pitsA = [0..5]

pitsB :: [Int]
pitsB = [7..12]

myStore :: Player -> Int
myStore PlayerA = storeA
myStore PlayerB = storeB

myPits :: Player -> [Int]
myPits PlayerA = pitsA
myPits PlayerB = pitsB

opposite :: Int -> Int
opposite i = 12 - i

mancalaInitialState :: GameState
mancalaInitialState = GameState [4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0] PlayerA

-- we used Player instead of String, but since they are just carried to be shown on screen, we will change to String so PvE can name the players anything we want
-- Bool means if the player currently on the move is having an extra move or not
data MessageInfo = TurnLogic String Bool | CaptureSignal String | ForbiddenPlay Int | Outcome (Maybe String)

turnText :: MessageInfo -> String
turnText (TurnLogic p False) = p ++ "'s Turn"
turnText (TurnLogic p True) = p ++ "'s Turn Again!!"
turnText (CaptureSignal p) = "BOOM => Capture Achieved by " ++ p
turnText (ForbiddenPlay (-1)) = "Invalid Input. Try Again..."
turnText (ForbiddenPlay _) =  "Illegal Move. Try Again..."
turnText (Outcome (Just p)) = "The Winner is " ++ p ++ "!!!"
turnText (Outcome Nothing) = "It's a Draw???"

drawBoard :: MessageInfo -> GameState -> String
--                 init (unlines (top ++ middle ++ bottom))    -- splits the drawing into three parts, all returning [String]
drawBoard msg gs = unlines (["\n"] ++ top ++ middle ++ bottom) -- preetier print when printing [GameState]
            where
                t = board gs
                top = indent (labelRow [6, 5, 4, 3, 2, 1] ++ sideBySide [pit (t !! i) | i <- [12, 11, 10, 9, 8, 7]])
                bottom = indent (sideBySide [pit (t !! i) | i <- [0, 1, 2, 3, 4, 5]] ++ [""] ++ labelRow [1, 2, 3, 4, 5, 6]) -- [""] represents the extra row to simulate the console print like at top
                blank = replicate pitsWidth ' '
                centreCol = [blank, blank, center pitsWidth (turnText msg), blank, blank] -- text inside the "announcement box" must be of len <= 53
                middle = [l ++ "  " ++ c ++ "  " ++ r | (l, c, r) <- zip3 (storeBox (t !! 13)) centreCol (storeBox (t !! 6))] -- list comprehension which concatenates rows of BoxA, centreCol and BoxB

instance Show GameState where
    show :: GameState -> String
    show gs = drawBoard (TurnLogic (show (onTurn gs)) False) gs 

changeAt :: Int -> (Int -> Int) -> [Int] -> [Int]
changeAt k f xs = [if i == k then f x else x | (i, x) <- zip [0..] xs]

validMoves :: GameState -> [Int]
validMoves gs = [i | i <- myPits (onTurn gs), t !! i /= 0]
            where
                t = board gs

nextIndex :: Player -> Int -> Int
nextIndex p k
            | p == PlayerA = (k + 1) `mod` 13
            | otherwise = if k == 5 then 7 else (k + 1) `mod` 14

-- players turn -> index of the current pit -> number of stones we traverse with -> current state of the board -> (index of the destination pit, new state of the board)
-- before the jump occurs we already incremented the value on the destination pit and adjust the other arguments
sow :: Player -> Int -> Int -> [Int] -> (Int, [Int])
sow _ k 0 t = (k, t)
sow p k q t = sow p l (q - 1) (changeAt l (+1) t)
            where
                l = nextIndex p k

freeTurn :: Player -> Int -> Bool
freeTurn p k = myStore p == k

-- players turn -> index of the destination pit at the end of sow -> state of the board at the end of sow -> state of the board after applyCapture
applyCapture :: Player -> Int -> [Int] -> [Int]
applyCapture p k t
                | notStores && correctPlayer && correctValues = chaining
                | otherwise = t
                where
                    valK = t !! k
                    valOppK = t !! opposite k
                    correctValues = valK == 1 && valOppK /= 0
                    correctPlayer = k `elem` myPits p
                    notStores = k /= storeA && k /= storeB
                    chaining = changeAt (myStore p) (\x -> x + valK + valOppK) (changeAt (opposite k) (const 0) (changeAt k (const 0) t))

-- chosen pit's index -> state of the board aswell as the player in turn -> new state of the board aswell as computed which players turn it is
{- HLINT ignore "Use uncurry" -}
playMove :: Int -> GameState -> GameState
playMove k gs = GameState endBoard (if freeTurn turn (fst sowed) then turn else opponent turn)
            where
                t = board gs
                turn = onTurn gs
                sowed = sow turn k (t !! k) (changeAt k (const 0) t)
                endBoard = applyCapture turn (fst sowed) (snd sowed)

isGameOver :: GameState -> Bool
isGameOver gs = determineQuantity pitsA == 0 || determineQuantity pitsB == 0
            where
                t = board gs
                determineQuantity = foldl (\acc x -> acc + t !! x) 0
                
whoWins :: GameState -> Maybe Player
whoWins gs
        | pointsA > pointsB = Just PlayerA
        | pointsA < pointsB = Just PlayerB
        | otherwise = Nothing
        where
            t = board gs
            pointsA = t !! storeA
            pointsB = t !! storeB

-- depth of expansion -> root GameState(can be any GameState) -> Rose tree representing every possible state for each possible move, up to the maximum of depth moves
gameTree :: Int -> GameState -> Rose GameState
gameTree 0 gs = Node gs []
gameTree d gs
            | isGameOver gs = Node gs []
            | otherwise = Node gs [gameTree (d - 1) (playMove i gs) | i <- validMoves gs]
