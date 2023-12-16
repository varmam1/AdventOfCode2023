{-# LANGUAGE OverloadedStrings #-}
import Data.List.Split
import Data.List
import Data.Text(pack, unpack, replace)
import Data.Function (on)

main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day05/input"
    let relevantBits = splitOn [""] (lines contents)
    let seeds = obtainSeeds(head (head relevantBits))
    let maps = parseAllMaps(removeUnnecessaryHeader(drop 1 relevantBits))
    let allLocs = allSeedLocs seeds maps
    print (minimum allLocs)

    -- Part 2
    -- The format is (initialVal, range)
    let seedIntervals = seedsToIntervals seeds
    let outInts = seedIntervalToLocation seedIntervals maps
    print(minOfListOfTuples(seedIntervalToLocation seedIntervals maps))

-- Gets rid of the line which is just a string mentioning which map it is
removeUnnecessaryHeader :: [[String]] -> [[String]]
removeUnnecessaryHeader = map (drop 1)

-- Given the first line, parses out the seeds as a list of Ints
obtainSeeds :: String -> [Int]
obtainSeeds firstLine = strToInt(splitOn " " (last(splitOn ":" ((unpack . replace ": " ":" . pack) firstLine))))

-- Given a string of format destination source range, parses that into a list of int tuples
parseIntervals :: [String] -> [(Int, Int, Int)]
parseIntervals [] = []
parseIntervals (x:xs) = 
    let vals = strToInt(splitOn " "  x) in
        (head vals, vals !! 1, vals !! 2) : parseIntervals xs

-- Iterates the above parsing over an entire map
parseAllMaps :: [[String]] -> [[(Int, Int, Int)]]
parseAllMaps = map parseIntervals

-- Given a list of intervals with destination/Source/range and the input, output
computeInterval :: [(Int, Int, Int)] -> Int -> Int
computeInterval [] val = val
computeInterval ((dest, source, range):intervals) val = 
    if val >= source && val < source + range then 
        val - source + dest 
    else 
        computeInterval intervals val

-- Takes a seed and all of the maps and returns the output
seedToLocation :: Int -> [[(Int, Int, Int)]] -> Int
seedToLocation = foldl (flip computeInterval)

-- Applies the above function for every seed in a list
allSeedLocs :: [Int] -> [[(Int, Int, Int)]] -> [Int]
allSeedLocs [] _ = []
allSeedLocs (seed:seeds) intervals = seedToLocation seed intervals : allSeedLocs seeds intervals

-- For pt 2 where its all intervals now
seedsToIntervals :: [Int] -> [(Int, Int)]
seedsToIntervals [] = []
seedsToIntervals (x1:x2:xs) = (x1, x2) : seedsToIntervals xs

-- Parse String as Int
strToInt :: [String] -> [Int]
strToInt = map (\ x -> read x :: Int)

-- Given a list of intervals, get the intervals the seedInterval becomes, checking each one
-- Cases:
-- 1. All within an interval; in this case, ((initSeed - source) + dest, seedRange) is new interval
-- 2. It's within no interval: (initSeed, seedRange)
-- 3. If the interval juts out on either left or right, we have to compute the new seed interval for what is in our current interval and then recurse over the others
computeIntervalOverIntervals :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int)]
computeIntervalOverIntervals [] x = [x] 
computeIntervalOverIntervals ((dest, source, intervalRange):intervals) (initSeed, seedRange)
    | initSeed >= source && initSeed + seedRange <= source + intervalRange =
        [((initSeed - source) + dest, seedRange)] -- All of the original seed interval is within an interval
    | initSeed < source && initSeed + seedRange > source + intervalRange = -- seed interval is completely on top of inter
        (dest, intervalRange) : computeIntervalOverIntervals intervals (initSeed, source - initSeed) ++ computeIntervalOverIntervals intervals (source + intervalRange, initSeed + seedRange - (source + intervalRange))
    | initSeed < source && initSeed + seedRange > source = -- seed interval juts out on the left (and potentially right)
        (initSeed, source - initSeed) : computeIntervalOverIntervals intervals (source, seedRange - (source - initSeed))
    | initSeed >= source && initSeed < source + intervalRange = -- seed interval juts out to the right
        ((initSeed - source) + dest, source + intervalRange - initSeed) : computeIntervalOverIntervals intervals (source + intervalRange, seedRange - (source + intervalRange - initSeed))
    | otherwise = computeIntervalOverIntervals intervals (initSeed, seedRange) -- The current interval will not transform it

-- Function just to iterate over seedIntervals
iterOverSeedIntervals :: [(Int, Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
iterOverSeedIntervals intervalMap = concatMap (computeIntervalOverIntervals intervalMap)

-- Main method to call; will iterate over maps; need to sort the maps by source
seedIntervalToLocation :: [(Int, Int)] -> [[(Int, Int, Int)]] -> [(Int, Int)]
seedIntervalToLocation = foldl(\seedIntervals intervalMap -> iterOverSeedIntervals (sortBy (compare `on` sec) intervalMap) seedIntervals)

minOfListOfTuplesHelper :: [(Int, Int)] -> Int -> Int
minOfListOfTuplesHelper [] minSoFar = minSoFar
minOfListOfTuplesHelper ((seed, _ ):xs) minSoFar = minOfListOfTuplesHelper xs (min minSoFar seed)

minOfListOfTuples :: [(Int, Int)] -> Int
minOfListOfTuples seeds = minOfListOfTuplesHelper seeds (maxBound :: Int)

totalAmount :: [(Int, Int)] -> Int
totalAmount [] = 0 
totalAmount ((_, x):xs) = x + totalAmount xs

firstOfAll :: [(Int, Int)] -> [Int]
firstOfAll [] = []
firstOfAll ((x, _) : xs) = x : firstOfAll xs

-- Use for the sorting 
sec :: (a, b, c) -> b
sec (_, b, _) = b
