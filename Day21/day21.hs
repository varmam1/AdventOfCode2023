{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Sequence
import Debug.Trace
import Data.Bifunctor

main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day21/input"
    let grid = lines contents
    let (sX, sY) = findS grid
    print (numSpots grid (sX, sY) 64)

    let v1 = numSpots grid (sX, sY) 65
    let v2 = numSpots grid (sX, sY) (65 + 131)
    let v3 = numSpots grid (sX, sY) (65 + 131*2)

    -- Cached values for my input
    -- let v1 = 3884
    -- let v2 = 34564
    -- let v3 = 95816

    print ("For 65 steps: " ++ show v1)
    print ("For 65 + 131 steps: " ++ show v2)
    print ("For 65 + 131*2 steps: " ++ show v3)

    -- 202300*131 + 65 = 26501365

    let a = (v1 - 2*v2 + v3) `div` 2
    let b = (-3*v1 + 4*v2 - v3) `div` 2
    let c = v1
    let n = 202300

    print ("Equation: " ++ show a ++ "n^2 + " ++ show b ++ "n + " ++ show c)

    print ("Total spots: " ++ show (a*n*n + b*n + c))


type Point = (Int, Int)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

findS :: [String] -> Point
findS grid = head (concatMap ((\row -> map ((\col -> (fst row, col)) . fst) (snd row)) . Data.Bifunctor.second (filter (\col -> snd col == 'S'))) (enumerate (map enumerate grid)))

checkPoint :: [String] -> Point -> [Point]
checkPoint grid (x, y) = 
    if getValueAt grid (x, y) == '#' then []
    else [(x, y)]

neighbors :: [String] -> Point -> [Point]
neighbors grid (x, y) = 
    checkPoint grid (x + 1, y) ++ checkPoint grid (x - 1, y) ++ checkPoint grid (x, y + 1) ++ checkPoint grid (x, y - 1)

numSpots :: [String] -> Point -> Int -> Int
numSpots grid p n = Set.size (takeSteps grid (Set.singleton p) n)

-- Finds the set of spots that can be gotten to for n steps
takeSteps :: [String] -> Set.Set Point -> Int -> Set.Set Point
takeSteps grid s 0 = s
takeSteps grid s n = 
    let set = takeSteps grid s (n - 1) in
        Set.fromList (concat (map (neighbors grid) (Set.toList (set))))

getValueAt :: [String] -> Point ->  Char
getValueAt grid (x, y) = grid !! (mod x (length grid)) !! (mod y (length (head grid)))