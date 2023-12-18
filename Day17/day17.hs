{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.PSQueue as PSQueue
import qualified Data.Map as Map
import Debug.Trace

main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day17/input"
    let graph = map (map digitToInt) (lines contents)
    print (part1 graph)

    -- Part 2
    print (part2 graph)

-- State definitions
data Direction = North | West | East | South deriving (Eq,Ord,Enum,Show)
type Point = (Int, Int)
type State = ((Point, Direction), Int)

-- Getter functions for a state
getPoint :: State -> Point
getPoint = fst . fst
getDirection :: State -> Direction
getDirection = snd . fst
getNumSteps :: State -> Int
getNumSteps = snd

-- Creates a state
makeState :: Point -> Direction -> Int -> State
makeState p d n = ((p, d), n)

-- Easier interface to get the value at a point
getValueAtPoint :: [[Int]] -> Point -> Int
getValueAtPoint g (x, y) = g !! x !! y

-- Priority Queue
-- Need to only call if distance is less than recorded
insertPQueue :: State -> Int -> PSQueue.PSQ State Int -> PSQueue.PSQ State Int
insertPQueue = PSQueue.insert

popFromPQueue :: PSQueue.PSQ State Int -> (State, PSQueue.PSQ State Int)
popFromPQueue pq = 
    case PSQueue.minView pq of
        Just (x, newPQ) -> (PSQueue.key x, newPQ)
        Nothing -> error (show pq)

-- Checks if out of bounds graph
outOfBounds :: [[Int]] -> Point -> Bool
outOfBounds graph (x, y) = x < 0 || y < 0 || x >= length graph || y >= length (head graph)

-- Finds the next point in the direction mentioned
applyDirection :: Direction -> Point -> Point
applyDirection dir (x, y) =
    case dir of
        North -> (x - 1, y)
        South -> (x + 1, y)
        East -> (x, y + 1)
        West -> (x, y - 1)

-- Gets the new state given the current point and direction and new amount of steps
getNewState :: [[Int]] -> Point -> Direction -> Int -> Int -> Int -> [State]
getNewState graph (x, y) dir n minSteps maxSteps  =
    if n > minSteps then 
        let newPt = applyDirection dir (x, y) in
            ([makeState newPt dir n | not (n > maxSteps || outOfBounds graph newPt)])
    else
        let newPt = iterate (applyDirection dir) (x, y) !! minSteps in 
            ([makeState newPt dir minSteps | not (outOfBounds graph newPt)])

-- Function for finding the neighbors given a state
frontier :: [[Int]] -> State -> Int -> Int -> [State]
frontier graph state minSteps maxSteps= do
    let dir = getDirection state
    let numSteps = getNumSteps state
    let p = getPoint state
    case dir of
        North -> getNewState graph p North (numSteps + 1) minSteps maxSteps ++ getNewState graph p  East minSteps minSteps maxSteps ++ getNewState graph p  West minSteps minSteps maxSteps
        South -> getNewState graph p South (numSteps + 1) minSteps maxSteps ++ getNewState graph p  East minSteps minSteps maxSteps ++ getNewState graph p  West minSteps minSteps maxSteps
        East  -> getNewState graph p  East (numSteps + 1) minSteps maxSteps ++ getNewState graph p North minSteps minSteps maxSteps ++ getNewState graph p South minSteps minSteps maxSteps
        West  -> getNewState graph p  West (numSteps + 1) minSteps maxSteps ++ getNewState graph p North minSteps minSteps maxSteps ++ getNewState graph p South minSteps minSteps maxSteps

-- Finds the sum of distances if any points were missed when getting doing the jump in part 2
extraIgnored :: [[Int]] -> State -> Int -> Int
extraIgnored graph (((x,y),dir),n) minSteps = 
    if n == minSteps then 
        case dir of
            North -> sum [getValueAtPoint graph (x + i, y) | i <- [1..(minSteps - 1)]]
            South -> sum [getValueAtPoint graph (x - i, y) | i <- [1..(minSteps - 1)]]
            East  -> sum [getValueAtPoint graph (x, y - i) | i <- [1..(minSteps - 1)]]
            West  -> sum [getValueAtPoint graph (x, y + i) | i <- [1..(minSteps - 1)]]
    else 0

-- Adds the states to the PQ and distances if the new distance makes sense (the main portion of the for loop)
getNewPQAndDistance :: [[Int]] -> [State] -> Int -> Int -> PSQueue.PSQ State Int -> Map.Map State Int -> (PSQueue.PSQ State Int, Map.Map State Int)
getNewPQAndDistance _ [] _ _ pq distances = (pq, distances)
getNewPQAndDistance graph (s:states) minSteps currentDistance pq distances = 
    let alt = currentDistance + getValueAtPoint graph (getPoint s) + extraIgnored graph s minSteps in 
        case Map.lookup s distances of
            Just d -> 
                if d > alt then
                    getNewPQAndDistance graph states minSteps currentDistance (insertPQueue s alt pq) (Map.insert s alt distances)
                else
                    getNewPQAndDistance graph states minSteps currentDistance pq distances
            Nothing -> getNewPQAndDistance graph states minSteps currentDistance (insertPQueue s alt pq) (Map.insert s alt distances)

-- Have to check every end state at the end to see
generateAllEndStates :: Point -> Int -> Int -> [State]
generateAllEndStates dest minSteps maxSteps = 
    [((dest, North), i) | i <- [minSteps..maxSteps]] ++ [((dest, South), i) | i <- [minSteps..maxSteps]] ++ [((dest, East), i) | i <- [minSteps..maxSteps]] ++ [((dest, West), i) | i <- [minSteps..maxSteps]]

-- Given all the end states, finds the minimal distance of them all
checkIfAnyInDistances :: [State] -> Map.Map State Int -> Int
checkIfAnyInDistances [] _ = maxBound :: Int
checkIfAnyInDistances (s:states) distances = 
    case Map.lookup s distances of
        Just d -> checkIfAnyInDistances states distances `min` d
        Nothing -> checkIfAnyInDistances states distances

-- Runs Dijkstra as mentioned on Wikipedia
dijkstra :: [[Int]] -> Point -> Int -> Int -> PSQueue.PSQ State Int -> Map.Map State Int -> Int
dijkstra graph dest minSteps maxSteps pq distances = 
    if PSQueue.null pq then checkIfAnyInDistances (generateAllEndStates dest minSteps maxSteps) distances
    else 
        let (state, restOfPQ) = popFromPQueue pq in
        let neighbors = frontier graph state minSteps maxSteps in
             case Map.lookup state distances of
                Just d -> 
                    let (newPQ, newDistances) = getNewPQAndDistance graph neighbors minSteps d restOfPQ distances in
                        dijkstra graph dest minSteps maxSteps newPQ newDistances
                Nothing -> -2

-- Interface to call part 1
part1 :: [[Int]] -> Int
part1 graph = dijkstra graph (length graph - 1, length (head graph) - 1) 1 3 (PSQueue.singleton (((0, 0), East), 0) 0) (Map.singleton (((0, 0), East), 0) 0)

-- Interface to call part 2
part2 :: [[Int]] -> Int
part2 graph = dijkstra graph (length graph - 1, length (head graph) - 1) 4 10 (PSQueue.singleton (((0, 0), East), 0) 0) (Map.singleton (((0, 0), East), 0) 0)
