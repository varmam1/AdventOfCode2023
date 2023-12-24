{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.List.Split
import Data.Function (on)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace

data Direction = U | L | D | R deriving (Eq, Show, Ord)
type Point = (Int, Int)

main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day23/input"
    let hike = lines contents
    let compressedGraph = compressGraph hike (0, 1) (0, 1) (length hike - 1, length (head hike) - 2) ((0, 1), D) 0 Map.empty
    print (maximum (findAllPaths compressedGraph (0, 1) (length hike - 1, length (head hike) - 2) 0))

    -- Part 2
    print (maximum (findAllPathsUndirected (getUndirectedGraph compressedGraph) (0, 1) (length hike - 1, length (head hike) - 2) [] 0))

-- Standard graph problem stuff
outOfBounds :: [String] -> Point -> Bool
outOfBounds graph (x, y) = x < 0 || y < 0 || x >= length graph || y >= length (head graph)

checkPoint :: [String] -> Point -> [Point]
checkPoint grid (x, y) = 
    if outOfBounds grid (x, y) || grid !! x !! y == '#' then []
    else [(x, y)]

neighbors :: [String] -> Point -> [Point]
neighbors grid (x, y) = 
    if grid !! x !! y == '#' then []
    else 
        checkPoint grid (x + 1, y) ++ checkPoint grid (x - 1, y) ++ checkPoint grid (x, y + 1) ++ checkPoint grid (x, y - 1)

-- Compresses the grid into a more usable graph where the nodes are decision points and the edges are how long is it in between 
compressGraph :: [String] -> Point -> Point -> Point -> (Point, Direction) -> Int -> Map.Map (Point, Direction) (Point, Int) -> Map.Map (Point, Direction) (Point, Int)
compressGraph grid currPoint lastVisited endPoint lastSeenDecisionPt distanceTravelled distanceMap = 
    if currPoint == endPoint then Map.insert lastSeenDecisionPt (endPoint, distanceTravelled) distanceMap
    else
        let frontier = delete lastVisited (neighbors grid currPoint) in
        if length frontier == 1 then compressGraph grid (head frontier) currPoint endPoint lastSeenDecisionPt (distanceTravelled + 1) distanceMap
        else 
            let newMap = Map.insert lastSeenDecisionPt (currPoint, distanceTravelled) distanceMap in
            -- CONDITION OF INPUT: There are only 2 paths to take here, right or down
            if elem (fst currPoint, snd currPoint + 1) frontier then 
                let newNewMap = compressGraph grid (fst currPoint, snd currPoint + 1) currPoint endPoint (currPoint, R) 1 newMap in
                    if elem (fst currPoint + 1, snd currPoint) frontier then
                        compressGraph grid (fst currPoint + 1, snd currPoint) currPoint endPoint (currPoint, D) 1 newNewMap
                    else
                        newNewMap
            else
                compressGraph grid (fst currPoint + 1, snd currPoint) currPoint endPoint (currPoint, D) 1 newMap

-- Finds all of the paths from the start to the end in the directed compressed graph
findAllPaths :: Map.Map (Point, Direction) (Point, Int) -> Point -> Point -> Int -> [Int]
findAllPaths compressedGraph currPoint endPoint currDistance =
    if currPoint == endPoint then [currDistance]
    else 
        if Map.member (currPoint, R) compressedGraph then
            let (nextPoint, dist) = compressedGraph Map.! (currPoint, R) in
            let rightPaths = findAllPaths compressedGraph nextPoint endPoint (currDistance + dist) in
            if Map.member (currPoint, D) compressedGraph then
                let (nextPointDown, distDown) = compressedGraph Map.! (currPoint, D) in
                rightPaths ++ (findAllPaths compressedGraph nextPointDown endPoint (currDistance + distDown))
            else
                rightPaths
        else 
            let (nextPointDown, distDown) = compressedGraph Map.! (currPoint, D) in
            findAllPaths compressedGraph nextPointDown endPoint (currDistance + distDown)

-- Helper to make it easier to find undirected graph
changeFormat :: Map.Map (Point, Direction) (Point, Int) -> Map.Map Point [(Point, Int)]
changeFormat directedGraph = 
    Map.fromList (map (
        \x -> 
            if Map.member (x, R) directedGraph then
                if Map.member (x, D) directedGraph then
                    (x, [directedGraph Map.! (x, R)] ++ [directedGraph Map.! (x, D)])
                else 
                    (x, [directedGraph Map.! (x, R)])
            else 
                (x, [directedGraph Map.! (x, D)])
    ) (map (fst . fst) (Map.toList directedGraph)))

-- Given the directed compressed graph, make it undirected
getUndirectedGraph :: Map.Map (Point, Direction) (Point, Int) -> Map.Map Point [(Point, Int)]
getUndirectedGraph directedGraph = 
    let forwardGraph = changeFormat directedGraph in
    -- Need to get the back arrows s.t v_i -> v_0 
    let backwardsGraph = Map.fromList (map (\v0 -> -- (Point, [(Point, Int)])
            let valuesToV0 = filter (\vi -> elem v0 (map fst (snd vi))) (Map.toList forwardGraph) in --[(Point, [(Point, Int)])]
            let valueToDistance = map (\v -> let dist = head (filter (\y -> v0 == fst y) (snd v)) in (fst v, snd dist)) valuesToV0 in (v0, valueToDistance)) (map (fst) (Map.toList forwardGraph))) in
    Map.unionWith (++) forwardGraph backwardsGraph

-- Finds the length of each of the paths in the undirected version of the graph
findAllPathsUndirected :: Map.Map Point [(Point, Int)] -> Point -> Point -> [Point] -> Int -> [Int]
findAllPathsUndirected undirectedGraph currPoint endPoint seenPoints currDistance =
    if currPoint == endPoint then [currDistance]
    else if elem currPoint seenPoints then []
    else applyToEach (undirectedGraph Map.! currPoint) undirectedGraph endPoint (currPoint:seenPoints) currDistance

-- Helper function to apply the findAllPathsUndirected function to each of the neighbors
applyToEach :: [(Point, Int)] -> Map.Map Point [(Point, Int)] -> Point -> [Point] -> Int -> [Int]
applyToEach [] _ _ _ _ = []
applyToEach ((pt, dist):pts) undirectedGraph endPoint seenPoints currDistance = 
    findAllPathsUndirected undirectedGraph pt endPoint seenPoints (currDistance + dist) ++ applyToEach pts undirectedGraph endPoint seenPoints currDistance