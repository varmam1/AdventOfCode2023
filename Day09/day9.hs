main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day09/input"
    let sequences = parseLines(lines contents)
    let diffLists = map getFullDifferenceList sequences
    print (sum(map getNextNumber diffLists))

    -- Part 2 
    print (sum(map getPrevNumber diffLists))

-- Generate the difference list for a sequence
getDifferenceList :: [Int] -> [Int]
getDifferenceList xs = zipWith (-) (tail xs) xs

-- Get all of the difference lists for all the way down to 0s
getFullDifferenceList :: [Int] -> [[Int]]
getFullDifferenceList xs = do
    let diffList = getDifferenceList xs
    if all (== 0) diffList then [xs, diffList]
    else xs : getFullDifferenceList diffList

-- Given the difference lists, generate the next number
getNextNumber :: [[Int]] -> Int
getNextNumber diffLists = sum (map last diffLists)

-- Part 2

-- Given the difference lists, generate the previous number
getPrevNumber :: [[Int]] -> Int
getPrevNumber = foldr ((-) . head) 0

-- Parsing 
parseLines :: [String] -> [[Int]]
parseLines = map (map (read :: String -> Int) . words)
