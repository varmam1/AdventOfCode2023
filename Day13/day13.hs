{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.List.Split

main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day13/input"
    let parsedLines = splitOn [""] (lines contents) -- parsedLines :: [[String]]
    let linesOfReflection = map getLinesOfReflection parsedLines
    print (sum(map getLineValue linesOfReflection))

    -- Part 2
    let smudgedLinesOfReflection = map getSmudgedLinesOfReflection parsedLines
    print (sum(map getLineValue smudgedLinesOfReflection))

-- Gets the value of the line of reflection as specified in the problem
getLineValue :: (Int, Int) -> Int
getLineValue (a, b) = if a == -1 then b*100 else a

-- Test both lines of reflection and return (vert, horiz)
getLinesOfReflection :: [String] -> (Int, Int)
getLinesOfReflection lines = (getHorizLineOfReflectionHelper (all (==True) . concat) (transpose lines) 1, getHorizLineOfReflectionHelper (all (==True) . concat) lines 1)

-- Test both lines of reflection and return (vert, horiz) for if there should be a smudge
getSmudgedLinesOfReflection :: [String] -> (Int, Int)
getSmudgedLinesOfReflection lines = (getHorizLineOfReflectionHelper hasSmudge (transpose lines) 1, getHorizLineOfReflectionHelper hasSmudge lines 1)

-- Iterates through the indices, if the strings are equal when you reflect, then it's the line of reflection
getHorizLineOfReflectionHelper :: ([[Bool]] -> Bool) -> [String] -> Int -> Int
getHorizLineOfReflectionHelper logic lines index =
    if index >= length lines then -1
    else
        let numLines = index `min` (length lines - index) in
        let firstPart = slice (index - numLines) (index - 1) lines in
        let secondPart = reverse(slice index (index + numLines - 1) lines) in
            if logic (getListEquality firstPart secondPart) then index
            else getHorizLineOfReflectionHelper logic lines (index + 1)

-- Checks there's exactly one False in a [[Bool]]; Used for part 2 only
hasSmudge :: [[Bool]] -> Bool
hasSmudge bools = (length . filter (==False) . concat) bools == 1

-- Slice function but inclusive on both ends; cringe pythonic code
slice :: Int -> Int -> [String] -> [String]
slice start end = take (end - start + 1) . drop start

-- Pairwise element equality for each character in a list of strings
getListEquality :: [String] -> [String] -> [[Bool]]
getListEquality = zipWith (zipWith (==))
