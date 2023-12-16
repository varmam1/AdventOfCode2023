{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.List.Split
import Data.Char
import Data.String.Utils
import qualified Data.Map as Map

main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day15/input"
    let string = splitOn "," (Data.String.Utils.rstrip contents)
    print (sum(map hash string))

    -- Part 2
    print (getBoxesVal (getBoxesState string Map.empty))

-- Hash function as mentioned in the problem
hash :: String -> Int
hash = foldl (\acc x -> (acc + ord x) * 17 `mod` 256) 0

-- Part 2:

-- Get the overall value of the box. Lotta maths
-- The map is of the box value -> [(str, lens power)] so just do \sum((val + 1) * \sum_i (i * lens))
getBoxesVal :: Map.Map Int [(String, Int)] -> Int
getBoxesVal = sum . map (\x -> (fst x + 1) * sum(zipWith (curry (\y -> fst y * snd(snd y))) [1..] (snd x))) . Map.toList

-- Creates the data structure needed to parse for the overall value
-- Checks if it's = or -; if = then find the hash value map and update the existing list by either updating the value or adding to the end
-- If -, removes it from the list if its there
-- In both cases, just creates a new Map since immutability and inserts it into the accumulator map
getBoxesState :: [String] -> Map.Map Int [(String, Int)] -> Map.Map Int [(String, Int)]
getBoxesState      [] m = m
getBoxesState (s:str) m = do
    let splittedPlus = splitOn "=" s
    let splittedMinus = splitOn "-" s
    if length splittedPlus == 2 then
        let strVal = head splittedPlus in
        let hashVal = hash strVal in
        case Map.lookup hashVal m of
            Just currentList -> getBoxesState str (Map.insert hashVal (updateValInList currentList (strVal, read (splittedPlus !! 1) :: Int) False) m)
            Nothing -> getBoxesState str (Map.insert hashVal [(strVal, read (splittedPlus !! 1) :: Int)] m)
    else case Map.lookup (hash (head splittedMinus)) m of
        Just x -> getBoxesState str (Map.insert (hash (head splittedMinus)) (removeFromList x (head splittedMinus)) m)
        Nothing -> getBoxesState str m

-- Removes a value from a list if it's there, otherwise identity function
removeFromList :: [(String, a)] -> String -> [(String, a)]
removeFromList     [] _ = []
removeFromList (x:xs) y = if fst x == y then removeFromList xs y else x : removeFromList xs y

-- Either updates the value in place or, if the value isn't there then adds to the end
updateValInList :: [(String, Int)] -> (String, Int) -> Bool -> [(String, Int)]
updateValInList     [] y foundVal = [y | not foundVal]
updateValInList (x:xs) y foundVal = if fst x == fst y then y : updateValInList xs y True else x : updateValInList xs y foundVal
