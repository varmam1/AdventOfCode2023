{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.List.Split
import qualified Data.Map as Map

main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day14/input"
    putStrLn (show(calculateLoad(northTilt (lines contents))))

    -- Part 2
    putStrLn (show(calculateLoad (getNthValue (lines contents) 1000000000)))

calculateLoad :: [String] -> Int
calculateLoad xs = sum(zipWith (*) (reverse [1..length(xs)]) (map (length . filter (=='O')) xs))

-- All of the tilt directions
northTilt :: [String] -> [String]
northTilt = transpose . westTilt . transpose
westTilt :: [String] -> [String]
westTilt = map tilt
southTilt :: [String] -> [String]
southTilt = transpose . eastTilt . transpose
eastTilt :: [String] -> [String]
eastTilt = map (reverse . tilt . reverse)

-- Tilt wrapper to call the helper
tilt :: String -> String
tilt str = tiltHelper str 0

-- We're actually just moving the dots to the end of each grouping between edges and # or between #s
tiltHelper :: String -> Int -> String
tiltHelper       "" numDots = replicate numDots '.'
tiltHelper ('.':xs) numDots = tiltHelper xs (numDots + 1)
tiltHelper ('#':xs) numDots = (replicate numDots '.') ++ ('#' : (tiltHelper xs 0))
tiltHelper   (x:xs) numDots =  x : (tiltHelper xs numDots)

-- Do the entire spin cycle 
doSpinCycle :: [String] -> [String]
doSpinCycle = eastTilt . southTilt . westTilt . northTilt

-- Find a cycle and return ((board, firstValSeeing), secondTime)
detectCycle :: Map.Map [String] Int -> [String] -> Int -> (([String], Int), Int)
detectCycle m xs 0 = detectCycle (Map.insert xs 0 m) xs 1
detectCycle m xs n = 
    let result = doSpinCycle (xs) in
        case Map.lookup result m of
            Nothing -> detectCycle (Map.insert result n m) result (n+1)
            Just x -> ((result, x), n)

-- Finds the cycle and then maths out what the remaining iterations should be
getNthValue :: [String] -> Int -> [String]
getNthValue xs n = do
    let cycle = detectCycle Map.empty xs 0
    let firstSeeing = snd(fst(cycle))
    let cycleLength = snd(cycle) - firstSeeing
    let remainingToDo = n - (firstSeeing + ((n - firstSeeing) `div` cycleLength)*cycleLength)
    (iterate (doSpinCycle) (fst(fst(cycle)))) !! remainingToDo