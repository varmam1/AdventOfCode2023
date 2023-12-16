{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.Bifunctor

main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day11/input"
    let spaceMap = lines contents
    let emptyCols = getEmptyCols spaceMap
    let emptyRows = getEmptyRows spaceMap
    let points = getAllSpots spaceMap
    print (sum(getAllDistances points emptyRows emptyCols 1))

    -- Part 2
    print (sum(getAllDistances points emptyRows emptyCols (1000000 - 1)))

-- Given a list of points, pair them up with each other
getAllPairs :: [a] -> [(a, a)]
getAllPairs [] = []
getAllPairs (point:points) = map (\p -> (point, p)) points ++ getAllPairs points

-- Given the points on the map, the empty rows and cols and how many extra rows/cols should be added, returns all distances
getAllDistances :: [(Int, Int)] -> [Int] -> [Int] -> Int ->[Int]
getAllDistances points emptyRows emptyCols expansionFactor = map (\pair -> uncurry distance pair emptyRows emptyCols expansionFactor) (getAllPairs points)

-- Gets all of the indices of # in the map
getAllSpots :: [String] -> [(Int, Int)]
getAllSpots spaceMap = concatMap ((\row -> map ((\col -> (fst row, col)) . fst) (snd row)) . Data.Bifunctor.second (filter (\col -> snd col == '#'))) (enumerate (map enumerate spaceMap))

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

-- Gets indices of empty columns
getEmptyCols :: [String] -> [Int]
getEmptyCols spaceMap = getEmptyRows (transpose spaceMap)

getEmptyRows :: [String] -> [Int]
getEmptyRows spaceMap =  map fst (filter (all(=='.') . snd) (enumerate spaceMap))

-- Gets the distance between points given the empty rows, cols and expansion factor (the amount of EXTRA rows/cols added)
distance :: (Int, Int) -> (Int, Int) -> [Int] -> [Int] -> Int -> Int
distance (x1, y1) (x2, y2) emptyRows emptyCols expansionFactor = abs(y2 - y1) + abs(x2 - x1)
    + length(filter (\x -> (x1 < x && x2 > x) || (x2 < x && x1 > x)) emptyRows) * expansionFactor
    + length(filter (\y -> (y1 < y && y2 > y) || (y2 < y && y1 > y)) emptyCols) * expansionFactor
