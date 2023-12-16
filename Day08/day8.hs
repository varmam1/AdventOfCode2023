{-# LANGUAGE OverloadedStrings #-}
import Data.List.Split
import Data.List
import Data.Text(pack, unpack, replace)
import Debug.Trace
import Data.Function (on)
import qualified Data.Map as Map
import Data.String.Utils (rstrip)

main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day08/input"
    let movements = Data.String.Utils.rstrip(head (lines contents))
    let graph = Data.List.drop 2 (lines contents)
    let mappedGraph = makeMapFromLines graph
    let startingVal = "AAA"
    print (checkStepsToZ mappedGraph startingVal movements 0 movements)

    -- Part 2
    let allEndWithAs = Data.List.filter (\x -> last x == 'A') (Map.keys mappedGraph)
    let steps = aToZSteps allEndWithAs mappedGraph movements
    print (Data.List.foldr lcm 1 steps)

-- For a bunch of starting values, finds the number of steps to an ending value on a single path
aToZSteps :: [String] -> Map.Map String (String, String) -> [Char] -> [Int]
aToZSteps [] _ _ = []
aToZSteps (a:allEndWithAs) mappedGraph movements = checkStepsToZ mappedGraph a movements 0 movements : aToZSteps allEndWithAs mappedGraph movements

-- Finds the number of steps to get from starting to ending value
-- Gets the map graph, the current value to iterate on, the movements to do, the amount of steps done so far, and then a repeat of the full movement string for later looping
checkStepsToZ :: Map.Map String (String, String) -> String -> [Char] -> Int -> [Char] -> Int
checkStepsToZ mappedGraph [_, _, 'Z'] movements steps movementForLooping = steps
checkStepsToZ mappedGraph currentVal [] steps movementForLooping = checkStepsToZ mappedGraph currentVal movementForLooping steps movementForLooping
checkStepsToZ mappedGraph currentVal (movement:movements) steps movementForLooping = 
    let (l, r) = (mappedGraph Map.! currentVal) in
    if movement == 'L' then checkStepsToZ mappedGraph l movements (steps + 1) movementForLooping
    else checkStepsToZ mappedGraph r movements (steps + 1) movementForLooping

-- Parsing 

makeMapFromLines :: [String] -> Map.Map String (String, String)
makeMapFromLines lines = Map.fromList (parseLines lines)

parseLines :: [String] -> [(String, (String, String))]
parseLines = map parseLine

parseLine :: String -> (String, (String, String))
parseLine line = do
    let splitLine = splitOn "=" (reformatLine line)
    let splitPair = splitOn "," (last splitLine)
    (head splitLine, (head splitPair, last splitPair))

reformatLine :: String -> String
reformatLine = unpack . replace " " "" . replace "(" "" . replace ")" "" . pack . Data.String.Utils.rstrip