{-# LANGUAGE OverloadedStrings #-}
import Data.List.Split
import Data.List
import Data.Text(pack, unpack, replace)
import Debug.Trace
import Data.List (sortBy)
import Data.Function (on)

-- need to calc (t_0 - t)t = d_0 => t**2 - t_0*t + d_0 = 0 

main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day6/input"
    let times = parseLine((lines contents) !! 0)
    let distances = parseLine((lines contents) !! 1)
    putStrLn (show(foldr (*) 1 (waysToSatisfy times distances)))

    -- Part 2
    let time = parseLineP2((lines contents) !! 0)
    let distance = parseLineP2((lines contents) !! 1)
    let ans = numberOfIntsBetweenRoots (getQuadraticRoots 1 (-(fromIntegral time)) (fromIntegral distance))
    putStrLn (show(ans))

parseLine :: String -> [Int]
parseLine line = map (\x -> read x :: Int) (filter (\x -> x /= "" ) (splitOn " " (last(splitOn ":" (line)))))

-- Assuming the input won't give us something with no vals
getQuadraticRoots :: Double -> Double -> Double -> (Double, Double)
getQuadraticRoots a b c =
   let d = b^2 - 4*a*c in
   ((-b - sqrt (d))/2 * (a), (-(b) + sqrt (d))/2 * (a)) -- Always give lowest val first

numberOfIntsBetweenRoots :: (Double, Double) -> Int
numberOfIntsBetweenRoots (a, b) = (ceiling b) - (ceiling a)

waysToSatisfy :: [Int] -> [Int] -> [Int]
waysToSatisfy [] [] = []
waysToSatisfy (time:times) (distance:distances) = (numberOfIntsBetweenRoots (getQuadraticRoots 1 (-(fromIntegral time)) (fromIntegral distance))) : (waysToSatisfy (times) (distances))

-- P2 Parse function
parseLineP2 :: String -> Int
parseLineP2 line = read (intercalate "" (filter (\x -> x /= "" ) (splitOn " " (last(splitOn ":" (line)))))) :: Int
