{-# LANGUAGE OverloadedStrings #-}
import Data.List.Split
import Data.List
import Data.Text(pack, unpack, replace)
import Debug.Trace

main :: IO()
main = do
    contents <- readFile "Day4/input"
    putStrLn (show(sum(part1(lines contents))))
    putStrLn (show(sum(getNumberOfCards(numberOfMatchesAllLines(lines(contents))))))

-- Part 1

-- For each line, we have two sets and want to find the intersection and then calculate 2^{n - 1} where n is the number of winning if n > 0
-- There are some double spaces due to single digit values so this would interfere with the split; need to reformat the line first to remove unnecessary spaces

reformatLine :: String -> String
reformatLine = unpack . replace " | " "|" . replace ": " ":" . replace "  " " " . pack

-- The following gets the number of matches given a string line
numberOfMatches :: String -> Int
numberOfMatches l = 
    let firstAndLastSet = splitOn "|" (last(splitOn ":" (reformatLine l))) in 
    length((splitOn " " (head firstAndLastSet)) `intersect` (splitOn " " (last firstAndLastSet)))


part1 :: [String] -> [Int]
part1 [] = []
part1 (l:ls) = (processLinePart1 l) : (part1 ls)

processLinePart1 :: String -> Int
processLinePart1 l = 
    let intersectLength = numberOfMatches l in 
    if intersectLength > 0 then 2 ^ (intersectLength - 1) else 0

-- Part 2

-- This gets the list of number of matches for every line
numberOfMatchesAllLines :: [String] -> [Int]
numberOfMatchesAllLines [] = []
numberOfMatchesAllLines (l:ls) = (numberOfMatches l) : (numberOfMatchesAllLines ls)

-- This gets the amount of each card that is generated
getNumberOfCards :: [Int] -> [Int]
getNumberOfCards matches = addToList matches (replicate (length(matches)) 1)

-- Iterates over the cards generated list and adds the amount of cards to each of the next n cards
addToList :: [Int] -> [Int] -> [Int]
addToList [] [] = []
addToList (match:matches) (card:cards) = card : (addToList (matches) (addValueToNextN card match cards))

-- Given the value to add and how many of the next indices to add to; adds val to the next n elements of the array
addValueToNextN :: Int -> Int -> [Int] -> [Int]
addValueToNextN val 0 cards = cards
addValueToNextN val n (card:cards) = (card + val) : (addValueToNextN val (n-1) (cards))
