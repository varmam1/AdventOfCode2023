main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day9/input"
    let sequences = parseLines(lines contents)
    let diffLists = map getFullDifferenceList sequences
    putStrLn (show(sum(map getNextNumber diffLists)))

    -- Part 2 
    putStrLn (show(sum(map getPrevNumber diffLists)))

-- Generate the difference list for a sequence
getDifferenceList :: [Int] -> [Int]
getDifferenceList (x_1 : x_2 : []) = [x_2 - x_1]
getDifferenceList (x_1 : x_2 : xs) = (x_2 - x_1) : getDifferenceList (x_2 : xs)

-- Get all of the difference lists for all the way down to 0s
getFullDifferenceList :: [Int] -> [[Int]]
getFullDifferenceList xs = do
    let diffList = getDifferenceList xs
    if all (\n -> n == 0) diffList then xs : diffList : []
    else xs : (getFullDifferenceList diffList)

-- Given the difference lists, generate the next number
getNextNumber :: [[Int]] -> Int
getNextNumber diffLists = sum (map last diffLists)

-- Part 2

-- Given the difference lists, generate the previous number
getPrevNumber :: [[Int]] -> Int
getPrevNumber diffLists = (foldr (-) 0 (map head diffLists))

-- Parsing 
parseLines :: [String] -> [[Int]]
parseLines = map (map (read :: String -> Int) . words)
