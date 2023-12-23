{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.List.Split
import Data.Function (on)
import qualified Data.Map as Map
import Debug.Trace
import qualified Data.Sequence as Sequence
import Data.Bifunctor

main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day22/input"
    let bricks = map parseLine (lines contents)
    let sortedBricks = sortBy (compare `on` getMinZ) bricks
    let highestMap = Map.fromList(zip [(x,y) | x <- [0..9], y <- [0..9]] (repeat (0, 0)))
    let supports = getSupportsAfterDrop (zip (map unfoldBrick sortedBricks) [1..]) highestMap
    print (length (filter id (map (safeToDisintegrate supports) [1..length sortedBricks])))

    -- Part 2
    print (sum(map (chainReaction supports [] . createQueue) [1..length sortedBricks]))

type Point = ((Int, Int), Int)
type Range = (Point, Point)

-- Parsing and Sorting functions

listToPoint :: [Int] -> Point
listToPoint [x, y, z] = ((x, y), z)

parseLine :: String -> Range
parseLine s =
    let splitted = splitOn "~" s in
        (listToPoint (map (read :: String -> Int) (splitOn "," (head splitted))), listToPoint (map (read :: String -> Int) (splitOn "," (last splitted))))

getMinZ :: Range -> Int
getMinZ ((_, z1), (_, z2)) = if z1 < z2 then z1 else z2

-- Given a range, gets where all of the bricks are for this piece
unfoldBrick :: Range -> [Point]
unfoldBrick (((x1, y1), z1), ((x2, y2), z2))
    | x2 - x1 /= 0 = zip (zip [x1..x2] (repeat y1)) (repeat z1)
    | y2 - y1 /= 0 = zip (zip (repeat x1) [y1..y2]) (repeat z1)
    | otherwise = zip (zip (repeat x1) (repeat y1)) [z1..z2]

-- Simulates the dropping of the bricks. Returns the new coordinates of each brick as well as what labeled bricks support it now
getSupportsAfterDrop :: [([Point], Int)] -> Map.Map (Int, Int) (Int, Int) -> [[Int]]
getSupportsAfterDrop [] _ = []
getSupportsAfterDrop ((brick, label):bricks) highestZ =
    let (highest, supports) = getHighestZ brick label highestZ 0 [] in
        let newBrick = updateHighest brick (highest + 1) in
            supports : getSupportsAfterDrop bricks (insertIntoMap (map fst brick) (maximum (map snd newBrick)) label highestZ)

-- If the brick is all on the same Z value straight use the new highest val, otherwise math it out
updateHighest :: [Point] -> Int -> [Point]
updateHighest pts newHighest
    | all (== snd (head pts)) (tail (map snd pts)) = zip (map fst pts) (repeat newHighest)
    | otherwise =
        let minZ = minimum (map snd pts) in
            map (Data.Bifunctor.second ((newHighest - minZ) +)) pts -- If the z's are different

-- Given the map of current highest by point, returns the highest point we have seen so far for a set of (x, y) as well as the labels that support the given brick
getHighestZ :: [Point] -> Int -> Map.Map (Int, Int) (Int, Int) -> Int -> [Int] -> (Int, [Int])
getHighestZ [] _ _ highest foundLabels = (highest, foundLabels)
getHighestZ (pt:brick) label highestZ highest foundLabels =
    let (val, correspondingLabel) = highestZ Map.! fst pt in
        if val > highest then getHighestZ brick label highestZ val [correspondingLabel]
        else if val < highest then getHighestZ brick label highestZ highest foundLabels
        else getHighestZ brick label highestZ val (correspondingLabel : foundLabels)

-- Insert the new highest Z and the label of the brick for eahc point
insertIntoMap :: [(Int, Int)] -> Int -> Int -> Map.Map (Int, Int) (Int, Int) -> Map.Map (Int, Int) (Int, Int)
insertIntoMap [] highest label highestZ = highestZ
insertIntoMap (pt:points) highest label highestZ = insertIntoMap points highest label (Map.insert pt (highest, label) highestZ)

-- Given the list of each label to the labels of what supports it, figure out whether we need the val label for any of the bricks
safeToDisintegrate :: [[Int]] -> Int -> Bool
safeToDisintegrate supports val = all (any (/= val)) supports

-- A nicer Queue interface rather than using Sequence.|> to push, purely for aesthetics
type Queue a = Sequence.Seq a
isQueueEmpty :: Queue a -> Bool
isQueueEmpty = Sequence.null

createQueue :: a -> Queue a
createQueue = Sequence.singleton

push :: Queue a -> a -> Queue a
push q n = q Sequence.|> n

pushAll :: Queue a -> [a] -> Queue a
pushAll = foldl push

pop :: Queue a -> (a, Queue a)
pop q =
    case Sequence.viewl q of
        Sequence.EmptyL -> error "AHHH"
        (x Sequence.:< q2) -> (x, q2)


-- For a value, deletes it from all the supports and adds any non-queued up labels which now have no supports to the queue (and the alreadyChecked)
chainReaction :: [[Int]] -> [Int] -> Queue Int -> Int
chainReaction supports alreadyChecked q =
    if isQueueEmpty q then length(filter (== []) supports)
    else
        let (val, updatedQ) = pop q in
        let newSupports = map (filter (/= val)) supports in
            let newIndices = map fst (filter (null . snd) (zip [1..] newSupports)) in
            chainReaction newSupports (newIndices ++ alreadyChecked) (pushAll updatedQ (filter (`notElem` alreadyChecked) newIndices))
