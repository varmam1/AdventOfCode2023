{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.List.Split
import qualified Data.Map as Map

main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day12/input"
    let parsedLines = map parseLine (lines contents)
    print (sum (map (uncurry getNumPossibilities) parsedLines))

    -- Part 2 
    let newStrings = map (duplicateString 5 . fst) parsedLines
    let newLists = map (duplicateList 5 . snd) parsedLines
    let newZipped = zip newStrings newLists
    print (sum (map (uncurry getNumPossibilities) newZipped))

duplicateList :: Int -> [Int] -> [Int]
duplicateList 1 l = l
duplicateList n l = (duplicateList (n - 1) l) ++ l

duplicateString :: Int -> String -> String
duplicateString 1 s = s
duplicateString n s = (s ++ "?") ++ duplicateString (n - 1) s

parseLine :: String -> (String, [Int])
parseLine s = let x = splitOn " " s in (head x, map (read :: String -> Int) (splitOn "," (last x)))

getNumPossibilities :: String -> [Int] -> Int
getNumPossibilities s groups = fst (getNumPossibilities' Map.empty s groups 0)

getNumPossibilities' :: Map.Map (String, [Int], Int) Int -> String -> [Int] -> Int -> (Int, Map.Map (String, [Int], Int) Int)
getNumPossibilities' memo      [] groups numConsecutive = 
    if length groups == 0 && numConsecutive == 0 then 
        (1, Map.insert ([], groups, numConsecutive) 1 memo) 
    else if length groups == 1 && head groups == numConsecutive then 
        (1, Map.insert ([], groups, numConsecutive) 1 memo) 
    else 
        (0, Map.insert ([], groups, numConsecutive) 0 memo) 
getNumPossibilities' memo ('.':s) groups numConsecutive =
    case Map.lookup ('.':s, groups, numConsecutive) memo of
        Just v -> (v, memo)
        Nothing -> 
            if numConsecutive == 0 then 
                let (v, m1) = getNumPossibilities' memo s groups 0 in
                    (v, Map.insert ('.':s, groups, numConsecutive) v m1)
            else if length groups > 0 && head groups == numConsecutive then 
                let (v, m1) = getNumPossibilities' memo s (tail groups) 0 in 
                    (v, Map.insert ('.':s, groups, numConsecutive) v m1)
            else (0, Map.insert ('.':s, groups, numConsecutive) 0 memo)
getNumPossibilities' memo ('#':s) groups numConsecutive = 
    case Map.lookup ('#':s, groups, numConsecutive) memo of
        Just v -> (v, memo)
        Nothing -> if length groups == 0 then (0, Map.insert ('#':s, groups, numConsecutive) 0 memo) 
        else 
            let (v, m1) = getNumPossibilities' memo s groups (numConsecutive + 1) in
                (v, Map.insert ('#':s, groups, numConsecutive) v m1)
getNumPossibilities' memo ('?':s) groups numConsecutive = 
    case Map.lookup ('?':s, groups, numConsecutive) memo of
        Just x -> (x, memo)
        Nothing -> 
            let (v1, m1) = getNumPossibilities' memo ('.':s) groups numConsecutive in
            let (v2, m2) = getNumPossibilities' m1 ('#':s) groups numConsecutive in
                (v1 + v2, Map.insert ('?':s, groups, numConsecutive) (v1 + v2) m2)
