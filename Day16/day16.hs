{-# LANGUAGE OverloadedStrings #-}
import Data.List
import qualified Data.Set as Set

data Direction = North | West | East | South deriving (Eq,Ord,Enum,Show)

main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day16/input"
    let mirrorMap = lines contents
    print (getTotalPointsEnergized (0, 0) East mirrorMap)

    -- Part 2
    let len = length mirrorMap - 1
    let inputList = zip (zip [0 .. len] (repeat 0)) (repeat East) ++ zip (zip [0 .. len] (repeat len)) (repeat West) ++ zip (zip (repeat 0) [0 .. len]) (repeat South) ++ zip (zip (repeat len) [0 .. len]) (repeat North)

    print (maximum (map (\x -> uncurry getTotalPointsEnergized x mirrorMap) inputList))

-- Given a point and the direction, finds the next point
findNextVal :: (Int, Int) -> Direction -> (Int, Int)
findNextVal (x, y) North = (x - 1, y) -- (Row, Col)
findNextVal (x, y)  West = (x, y - 1) -- (Row, Col)
findNextVal (x, y)  East = (x, y + 1) -- (Row, Col)
findNextVal (x, y) South = (x + 1, y) -- (Row, Col)

-- Checks if out of bounds
outOfBounds :: [String] -> (Int, Int) -> Bool
outOfBounds mirrorMap (x, y) = x < 0 || y < 0 || x >= length mirrorMap || y >= length(head mirrorMap)

-- Calculates total points energized given starting point and initial direction
getTotalPointsEnergized :: (Int, Int) -> Direction -> [String] -> Int
getTotalPointsEnergized startingPoint dir mirrorMap = length(Set.map fst (parseThroughMap mirrorMap startingPoint dir Set.empty)) -- Remove directions from Set since we only care about distinct points

-- Parses through the thing, recording all of the points and directions in the seen Set
parseThroughMap :: [String] -> (Int, Int) -> Direction -> Set.Set ((Int, Int), Direction) -> Set.Set ((Int, Int), Direction)
parseThroughMap mirrorMap (x, y) dir seen =
    if outOfBounds mirrorMap (x, y) || Set.member ((x, y), dir) seen then seen
    else 
        let value = (mirrorMap !! x) !! y in
        case value of
            '.' -> forward newSet
            '/' -> case dir of
                East  -> north newSet
                West  -> south newSet
                North -> east  newSet
                South -> west  newSet
            '-' -> case dir of 
                East  -> forward newSet
                West  -> forward newSet
                North -> west (updatedSet east)
                South -> west (updatedSet east)
            '|' -> case dir of
                East  -> north (updatedSet south)
                West  -> north (updatedSet south)
                North -> forward newSet
                South -> forward newSet
            '\\' -> case dir of
                East  -> south newSet
                West  -> north newSet
                North -> west  newSet
                South -> east  newSet
        where forward = parseThroughMap mirrorMap (findNextVal (x, y) dir) dir
              north = parseThroughMap mirrorMap (findNextVal (x, y) North) North
              south = parseThroughMap mirrorMap (findNextVal (x, y) South) South
              east = parseThroughMap mirrorMap (findNextVal (x, y) East) East
              west = parseThroughMap mirrorMap (findNextVal (x, y) West) West
              newSet = Set.insert ((x, y), dir) seen
              updatedSet = \d -> Set.insert ((x, y), dir) (d newSet)