{-# LANGUAGE OverloadedStrings #-}
import Data.List

main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day11/input"
    let spaceMap = lines contents
    let emptyCols = getEmptyCols spaceMap
    let emptyRows = getEmptyRows spaceMap
    let points = getAllSpots spaceMap
    putStrLn (show(sum(getAllDistances points emptyRows emptyCols 1)))

    -- Part 2
    putStrLn (show(sum(getAllDistances points emptyRows emptyCols (1000000 - 1))))

-- Given a list of points, pair them up with each other
getAllPairs :: [a] -> [(a, a)]
getAllPairs [] = [] 
getAllPairs (point:points) = (map (\p -> (point, p)) points) ++ getAllPairs (points)

-- Given the points on the map, the empty rows and cols and how many extra rows/cols should be added, returns all distances
getAllDistances :: [(Int, Int)] -> [Int] -> [Int] -> Int ->[Int]
getAllDistances points emptyRows emptyCols expansionFactor = map (\pair -> distance (fst(pair)) (snd(pair)) emptyRows emptyCols expansionFactor) (getAllPairs points)

-- Gets all of the indices of # in the map
getAllSpots :: [String] -> [(Int, Int)]
getAllSpots spaceMap = concat (map (\row -> map (\col -> (fst row, col))(map (\col -> fst(col)) (snd row))) (map (\row -> (fst(row), filter (\col -> snd(col) == '#') (snd(row)))) (enumerate (map enumerate spaceMap))))

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

-- Gets indices of empty columns
getEmptyCols :: [String] -> [Int]
getEmptyCols spaceMap = getEmptyRows (transpose spaceMap)

getEmptyRows :: [String] -> [Int]
getEmptyRows spaceMap =  map fst (filter (\row -> (all(=='.')) (snd(row))) (enumerate spaceMap))

-- Gets the distance between points given the empty rows, cols and expansion factor (the amount of EXTRA rows/cols added)
distance :: (Int, Int) -> (Int, Int) -> [Int] -> [Int] -> Int -> Int
distance (x1, y1) (x2, y2) emptyRows emptyCols expansionFactor = abs(y2 - y1) + abs(x2 - x1) 
    + length(filter (\x -> (x1 < x && x2 > x) || (x2 < x && x1 > x)) emptyRows) * expansionFactor
    + length(filter (\y -> (y1 < y && y2 > y) || (y2 < y && y1 > y)) emptyCols) * expansionFactor
