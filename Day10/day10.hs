{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.Maybe

main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day10/input"
    let fullMap = lines contents
    let (rowOfS, colOfS) = findIndexOfS fullMap 0
    let nextIndex = findNextValAfterS (fullMap) (rowOfS, colOfS)
    let totalLength = findNumSteps (fullMap) nextIndex (rowOfS, colOfS) 1
    putStrLn (show(totalLength `div` 2))
    
    -- Part 2
    let allLoopIndices = findAllLoopElems fullMap nextIndex (rowOfS, colOfS)
    let loopMap = reverse(generateLoopMap allLoopIndices (length fullMap) fullMap) -- Generates the map again with just the loop
    putStrLn (show(sum(map (getInsideLoopInLineWrapper (findValOfS loopMap (rowOfS, colOfS))) loopMap)))

-- Finds total length of the loop
findNumSteps :: [String] -> (Int, Int) -> (Int, Int) -> Int -> Int
findNumSteps map (currentRow, currentCol) (lastRow, lastCol) stepsTaken 
    | (map !! currentRow) !! currentCol == 'S' = stepsTaken
    | otherwise = findNumSteps map (findNextStep map (currentRow, currentCol) (lastRow, lastCol)) (currentRow, currentCol) (stepsTaken + 1)

-- Parses the current element, figures out where we came from and where we go next
findNextStep :: [String] -> (Int, Int) -> (Int, Int) -> (Int, Int)
findNextStep map (currentRow, currentCol) (lastRow, lastCol) 
    | currentVal == 'F' = 
        if lastRow == currentRow + 1 then (currentRow, currentCol + 1)
        else (currentRow + 1, currentCol)
    | currentVal == 'L' = 
        if lastRow == currentRow - 1 then (currentRow, currentCol + 1)
        else (currentRow - 1, currentCol)
    | currentVal == '7' = 
        if lastRow == currentRow + 1 then (currentRow, currentCol - 1)
        else (currentRow + 1, currentCol)
    | currentVal == 'J' = 
        if lastRow == currentRow - 1 then (currentRow, currentCol - 1)
        else (currentRow - 1, currentCol)
    | currentVal == '|' = 
        if lastRow == currentRow - 1 then (currentRow + 1, currentCol)
        else (currentRow - 1, currentCol)
    | otherwise = 
        if lastCol == currentCol - 1 then (currentRow, currentCol + 1)
        else (currentRow, currentCol - 1)
    where currentVal = (map !! currentRow) !! currentCol

-- Returns the index of the S
findIndexOfS :: [String] -> Int -> (Int, Int)
findIndexOfS (s:strings) row
    | sIndex == -1 = findIndexOfS strings (row + 1)
    | otherwise = (row, sIndex)
    where sIndex = fromMaybe (-1) (elemIndex 'S' s)

-- Find out what value S correponds to
findValOfS :: [String] -> (Int, Int) -> Char
findValOfS map (row, col)
    | north && east = 'L'
    | north && south = '|'
    | north && west = 'J'
    | east && south = 'F'
    | east && west = '-'
    | otherwise = '7'
    where north = (((map !! (row - 1)) !! col) == '|') || (((map !! (row - 1)) !! col) == 'F') || (((map !! (row - 1)) !! col) == '7')
          south = (((map !! (row + 1)) !! col) == '|') || (((map !! (row + 1)) !! col) == 'L') || (((map !! (row + 1)) !! col) == 'J')
          west = (((map !! row) !! (col + 1)) == '-') || (((map !! row) !! (col + 1)) == '7') || (((map !! row) !! (col + 1)) == 'J')
          east = (((map !! row) !! (col - 1)) == '-') || (((map !! row) !! (col - 1)) == 'L') || (((map !! row) !! (col - 1)) == 'F')

-- Finds out the next index from the S given there's 2 options (arbitrarily picks one)
findNextValAfterS :: [String] -> (Int, Int) -> (Int, Int)
findNextValAfterS map (row, col)
    | (((map !! (row - 1)) !! col) == '|') || (((map !! (row - 1)) !! col) == 'F') || (((map !! (row - 1)) !! col) == '7') = (row - 1, col)
    | (((map !! (row + 1)) !! col) == '|') || (((map !! (row + 1)) !! col) == 'L') || (((map !! (row + 1)) !! col) == 'J') = (row + 1, col)
    | (((map !! row) !! (col + 1)) == '-') || (((map !! row) !! (col + 1)) == '7') || (((map !! row) !! (col + 1)) == 'J') = (row, col + 1)
    | otherwise = (row, col - 1)

-- Part 2 Functions

-- Wrapper for the main function
getInsideLoopInLineWrapper :: Char -> String -> Int
getInsideLoopInLineWrapper sVal line  = getInsideLoopInLine line '.' 0 0 sVal

-- Given a line, counts how many of the characters are actually in the loop
getInsideLoopInLine :: String -> Char -> Int -> Int -> Char -> Int
getInsideLoopInLine [] _ numInside  _  _= numInside
getInsideLoopInLine (val:line) previousCorner numInside numPipesSoFar sVal
    | val == '|' = getInsideLoopInLine line previousCorner numInside (numPipesSoFar + 1) sVal
    | val == 'F' || val == 'L' = getInsideLoopInLine line val numInside (numPipesSoFar + 1) sVal
    | (val == '7' && previousCorner == 'F')  || (val == 'J' && previousCorner == 'L') = 
        getInsideLoopInLine line val numInside (numPipesSoFar - 1) sVal
    | val == '.' && (numPipesSoFar `mod` 2) == 1 = getInsideLoopInLine line previousCorner (numInside + 1) numPipesSoFar sVal
    | val == 'S' = getInsideLoopInLine (sVal:line) previousCorner numInside numPipesSoFar sVal
    | otherwise = getInsideLoopInLine line previousCorner numInside numPipesSoFar sVal

-- Gets all of the indices that are part of the loop
findAllLoopElems :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
findAllLoopElems map (currentRow, currentCol) (lastRow, lastCol) 
    | (map !! currentRow) !! currentCol == 'S' = [(currentRow, currentCol)]
    | otherwise = 
        (currentRow, currentCol) : (findAllLoopElems map (findNextStep map (currentRow, currentCol) (lastRow, lastCol)) (currentRow, currentCol))

-- Generates the map which just has the loop and if its not a part of the loop, it's a .
generateLoopMap :: [(Int, Int)] -> Int -> [String] -> [String]
generateLoopMap indices (-1)  _ = []
generateLoopMap indices row origMap = (getLoopStr (map (snd) (filter (\(x, _) -> x == row) indices)) (replicate 140 '.') (origMap !! row)) : (generateLoopMap indices (row - 1) origMap)

getLoopStr :: [Int] -> String -> String -> String
getLoopStr [] str  _ = str
getLoopStr (index:indices) str origLine = getLoopStr (indices) (replaceCharAtIndex index (origLine !! index) str) origLine

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

replaceCharAtIndex :: Int -> Char -> String -> String
replaceCharAtIndex index replacement str = strHead ++ replacement : safeTail strAfter
    where (strHead, strAfter) = splitAt index str
