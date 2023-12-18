{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.List.Split
import Numeric

data Direction = U | D | L | R deriving (Eq, Ord, Show, Read)

main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day18/input"
    let parsedLines = map parseLine (lines contents)
    let numBoundary = sum(map (snd . fst) parsedLines)
    let loop = (0, 0) : getPointsOnArray (map fst parsedLines) (0, 0)
    print (getNumSquares loop numBoundary)

    -- Part 2
    -- Parse the String to new (Direction, Int) pairs
    let newParsedLines = map (decodeHex . snd ) parsedLines
    let numBoundary2 = sum(map snd newParsedLines)
    let loop2 = (0, 0) : getPointsOnArray newParsedLines (0, 0)
    print (getNumSquares loop2 numBoundary2)

getPointsForInstr :: (Direction, Int) -> (Int, Int) -> (Int, Int)
getPointsForInstr (dir, n) (x, y) = 
    case dir of
        U -> (x - n, y)
        D -> (x + n, y)
        L -> (x, y - n)
        R -> (x, y + n)

getPointsOnArray :: [(Direction, Int)] -> (Int, Int) -> [(Int, Int)]
getPointsOnArray          []     _ = []
getPointsOnArray (i:instrs) (x, y) = let point = getPointsForInstr i (x, y) in point : getPointsOnArray instrs point

parseLine :: String -> ((Direction, Int), String)
parseLine l = let splitted = splitOn " " l in ((read (head splitted) :: Direction, read (splitted !! 1) :: Int), take 6 (drop 2 (last splitted)))

-- Precondition: The set of points is ordered and it starts and ends with the same point
getInternalArea :: [(Int, Int)] -> Int
getInternalArea                         [_] = 0
getInternalArea ((x1, y1) : (x2, y2) : pts) = (-x1*y2 + x2*y1) + getInternalArea ((x2, y2) : pts)

-- Use shoelace to find internal area, use pick's to find num of internal points
-- A = i + b/2 - 1 -> i = A + 1 - b/2 -> numSquares = i + b = A + 1 + b/2
getNumSquares :: [(Int, Int)] -> Int -> Int
getNumSquares loop boundaryPoints = (getInternalArea loop + boundaryPoints + 2) `div` 2 

decodeHex :: String -> (Direction, Int)
decodeHex s = do
    let dir = last s
    let steps = fst(head (readHex (init s)))
    case dir of
        '0' -> (R, steps)
        '1' -> (D, steps)
        '2' -> (L, steps)
        '3' -> (U, steps)
