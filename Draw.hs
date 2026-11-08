module Draw where

-- builds a string which has a set width and the text in the middle of it
center :: Int -> String -> String
center w s = replicate left ' ' ++ s ++ replicate right ' '
  where
    total = w - length s
    right = total `div` 2
    left  = total - right

-- represents the width of each row that constructs the pit on the screen, NOT A CONTRACT
pitWidth :: Int
pitWidth = 8

-- represents how many blank columns sideBySide puts between two neighbouring pits
pitGap :: Int
pitGap = 1

-- width of each row starting from horizontally the first pit and ending at horizontally the last pit 
pitsWidth :: Int
pitsWidth = 6 * pitWidth + 5 * pitGap -- 53

-- margin is the padding from column 0 of the screen up to the leftmost pit, the empty space before the first pit, NOT A CONTRACT
margin :: Int
margin = 10

pit :: Int -> [String]
pit n =
  [ "  ____  "
  , " /    \\ "
  , " |" ++ center 4 (show n) ++ "| "
  , " \\____/ " ]

storeBox :: Int -> [String]
storeBox n =
  [ " ------ "
  , "|      |"
  , "|" ++ center 6 (show n) ++ "|"
  , "|      |"
  , " ------ " ]

-- this function constructs the given boxes in a horizontal order
-- boxes represent pits, which must be of same height
-- i represents each row we construct
-- the concat argument basically constructs the horizontal row for each box b on the i'th row
-- concat then makes it all a String and we drop the starting pitGap we added, because we don't want it
sideBySide :: [[String]] -> [String]
sideBySide boxes = [drop pitGap (concat [replicate pitGap ' ' ++ b !! i | b <- boxes]) | i <- [0 .. length (head boxes) - 1]]

-- this function is used to give sideBySide a [[String]], which has inside [] a single String/row, meaning an aligned markers for each pit(aligned because of center function)
-- ks represents the markers that we want to create
-- sideBySide will only iterate with i = 0 and it is used here to just apply the pitGap alignment and to concat into one string
labelRow :: [Int] -> [String]
labelRow ks = sideBySide [[center pitWidth (show k)] | k <- ks]


-- this function is used on the top and bottom part of the drawing, where the pits are located
-- it just adds the margin to each row
indent :: [String] -> [String]
indent = map (replicate margin ' ' ++)
