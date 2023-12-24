{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.List.Split
import Data.Function (on)
import qualified Data.Map as Map
import Debug.Trace

main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day24/input"
    let vectors = map parseLine (lines contents)
    print (length(filter (== True) (map collides (pairs vectors))))
    -- r = r_0 + tv is equation of line

    -- Part 2 In Python using Z3

-- Gets all distinct pairs of lines
pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

type Vector = (Int, Int, Int)
getX :: Vector -> Int
getX (x, y, z) = x

getY :: Vector -> Int
getY (x, y, z) = y

getZ :: Vector -> Int
getZ (x, y, z) = z

listToVec :: [Int] -> Vector
listToVec [x, y, z] = (x, y, z)

parseLine :: String -> (Vector, Vector)
parseLine l =
    let splitPosVelo = splitOn " @ " l in
    (listToVec (map (read :: String -> Int) (splitOn ", " (head splitPosVelo))), listToVec (map (read :: String -> Int) (splitOn ", " (last splitPosVelo))))

-- Checks if determinant is 0
isParallelXY :: Vector -> Vector -> Bool
isParallelXY v0 v1 = getX v0 * getY v1 == getX v1 * getY v0

-- Given t, finds the coordinate at time t for a line
outOfBounds :: Vector -> Float -> Vector -> Bool
outOfBounds pos scalar velo =
    let x = fromIntegral  (getX pos) + scalar * fromIntegral  (getX velo) in
    let y = fromIntegral  (getY pos) + scalar * fromIntegral  (getY velo) in
    x >= 200000000000000 && y >= 200000000000000 && x <= 400000000000000 && y <= 400000000000000

-- Given two position and velocity vectors figures out if they collide in the x, y plane using cramer's rule
collides :: ((Vector, Vector), (Vector, Vector)) -> Bool
collides ((x0, v0), (x1, v1)) =
    not (isParallelXY v0 v1) && (
    -- tv_{0x} - sv_{1x} = x_1 - x_0
    -- tv_{0y} - sv_{1y} = y_1 - y_0
    -- at + bs = c
    -- dt + es = f
    let a = getX v0 in
    let b = - getX v1 in
    let c = getX x1 - getX x0 in
    let d = getY v0 in
    let e = - getY v1 in
    let f = getY x1 - getY x0 in
    let det = a*e - b*d in
    let detX = c*e - b*f in
    let detY = a*f - c*d in
    let t = fromIntegral detX / fromIntegral det in
    let s = fromIntegral detY / fromIntegral det in
    not (t < 0 || s < 0) && outOfBounds x0 t v0)

