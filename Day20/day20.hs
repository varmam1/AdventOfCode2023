{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Sequence
import Debug.Trace

main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day20/input"
    let (states, paths) = parseLines (lines contents) Map.empty Map.empty
    let updatedState = instantiateConjunctions states paths
    let (low, high) = processNPulses updatedState paths 1000 (0, 0)
    print (low * high)

    -- Part 2
    let cycles = getAllFourCycles paths
    let cycleEndNodeTemp = getFourConjunctions paths
    let cycleEndNode = map (\endNode -> head (getAllInputs endNode paths)) cycleEndNodeTemp
    print (foldr (lcm . processPulsesTillEnd 1 updatedState paths) 1 cycleEndNode)


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

-- Create datatypes for the FlipFlop and Conjunction Modules and have both as a Module type

data Signal = Low | High | None deriving (Eq, Show)

data FlipFlop = FlipFlop
    {
        ffName :: String,
        on :: Bool -- Whether on or off
    } deriving (Eq, Show)

applySignalFF :: FlipFlop -> Signal -> (FlipFlop, Signal)
applySignalFF f High = (f, None)
applySignalFF (FlipFlop n o) Low = (FlipFlop n (not o), if o then Low else High)

data Conjunction = Conjunction
    {
        cName :: String,
        lastInputPulse :: Map.Map String Signal -- Records last recorded signal of the input
    } deriving (Eq, Show)

applySignalConjunction :: Conjunction -> (String, Signal) -> (Conjunction, Signal)
applySignalConjunction (Conjunction c lastPulses) (inputModuleName, sig) =
    let newLastPulses = Map.insert inputModuleName sig lastPulses in
        let allHigh = all (== High) (Map.elems newLastPulses) in
            if allHigh then (Conjunction c newLastPulses, Low) else (Conjunction c newLastPulses, High)

data Module = FF FlipFlop | C Conjunction | Broadcast | Button deriving (Eq, Show)

getModuleName :: Module -> String
getModuleName (FF n) = ffName n
getModuleName (C n) = cName n
getModuleName Broadcast = "broadcaster"

type State = Map.Map String Module
type Paths = Map.Map String [String]

type Instruction = ((String, Signal), String) -- Source name -> Destination modules with what signal

-- Parses all lines into it
parseLines :: [String] -> State -> Paths -> (State, Paths)
parseLines [] m p = (m, p)
parseLines (l:ls) m p = uncurry (parseLines ls) (parseLine l m p)

-- Parses a line into a State and Paths map
parseLine :: String -> State -> Paths -> (State, Paths)
parseLine ('b':s) m p = (Map.insert "broadcaster" Broadcast m, Map.insert "broadcaster" (splitOn ", " (last (splitOn " -> " s))) p)
parseLine ('%':s) m p =
    let splitLine = splitOn " -> " s in
    (Map.insert (head splitLine) (FF (FlipFlop (head splitLine) False)) m, Map.insert (head splitLine) (splitOn ", " (last splitLine)) p)
parseLine ('&':s) m p =
    let splitLine = splitOn " -> " s in
    (Map.insert (head splitLine) (C (Conjunction (head splitLine) Map.empty)) m, Map.insert (head splitLine) (splitOn ", " (last splitLine)) p)

-- The following instantiates each of the Conjunction maps to have the inputs set at Low
instantiateConjunctions :: State -> Paths -> State
instantiateConjunctions st p = Map.map (generateConjMap p) st

-- Given a conjunction, will update the list to have all the inputs in it and instantiate them all to Low
generateConjMap :: Paths -> Module -> Module
generateConjMap p (C (Conjunction conjName pulses)) = C (Conjunction conjName (Map.fromList(zip (getAllInputs conjName p) (repeat Low))))
generateConjMap _ m = m

-- Gets all the input modules for a conjunction
getAllInputs :: String -> Paths -> [String]
getAllInputs name p = Map.keys (Map.filter (elem name) p)

-- Given the source module and the signal and the destination module, applies the necessary logic and returns the dest module with the signal it will output
applyModule :: String -> Signal -> Module -> (Module, Signal)
applyModule oldM sig (C c) =
    let (conj, newSig) = applySignalConjunction c (oldM, sig) in (C conj, newSig)
applyModule oldM sig (FF ff) =
    let (newFF, newSig) = applySignalFF ff sig in (FF newFF, newSig)
applyModule _ _ Broadcast = (Broadcast, Low)

-- Presses the button n times and returns low and high values
processNPulses :: State -> Paths -> Int -> (Int, Int) -> (Int, Int)
processNPulses state paths 0 (low, hi) = (low, hi)
processNPulses state paths n (low, hi) =
    let ((newLo, newHi), newState) = processPulse (createQueue (("Button", Low), "broadcaster")) state paths low hi in
        processNPulses newState paths (n-1) (newLo, newHi)

-- Does one pulse of the button and returns the resulting state and low and high values
processPulse :: Queue Instruction -> State -> Paths -> Int -> Int -> ((Int, Int), State)
processPulse q state paths low high =
    if isQueueEmpty q then ((low, high), state)
    else
        let (((oldModule, sig), newModuleName), updatedQueue) = pop q in
            if sig == None then processPulse updatedQueue state paths low high
            else
                case Map.lookup newModuleName state of
                    Just newModule ->
                        let (updatedNewModule, outputtedSignal) = applyModule oldModule sig newModule in
                        let pushedUpdatedQueue = pushAll updatedQueue (affectedModules paths newModuleName outputtedSignal) in
                        let toApply = processPulse pushedUpdatedQueue (Map.insert newModuleName updatedNewModule state) paths in
                        if sig == Low then
                            toApply (low + 1) high
                        else
                            toApply low (high + 1)
                    Nothing -> -- We're on the RX node Which has no actual Module
                        if sig == Low then
                            processPulse updatedQueue state paths (low + 1) high
                        else
                            processPulse updatedQueue state paths low (high + 1)

-- Given the module and the Paths map as well as the outputting signal from the module, returns a list of instructions from this module to it's dependencies
affectedModules :: Paths -> String -> Signal -> [Instruction]
affectedModules paths moduleName outputtedSignal =
    case Map.lookup moduleName paths of
        Just x -> map (\modu -> ((moduleName, outputtedSignal), modu)) x
        Nothing -> []

-- Part 2 Functions 
-- Can get the backwards of rx which should be a Conjunction with 4 inputs; For each input we can get the paths from broadcaster to it which should give us the nodes in each cycle
getFourConjunctions :: Paths -> [String]
getFourConjunctions paths = getAllInputs (head (getAllInputs "rx" paths)) paths

-- Gets the nodes in a cycle
getAllNodes :: Paths -> String -> [String]
getAllNodes paths endNode =
    let conj = head (getAllInputs endNode paths) in
        Set.toList (Set.fromList (delete endNode (getAllInputs conj paths ++ (paths Map.! conj) ++ [conj])))

-- Gets all the nodes in each of the 4 cycles
getAllFourCycles :: Paths -> [[String]]
getAllFourCycles paths =
    map (getAllNodes paths) (getFourConjunctions paths)

-- Want to process pulses until we see a Low from the endNode
processPulsesTillEnd :: Int -> State -> Paths -> String -> Int
processPulsesTillEnd n cycleState cyclePaths endNode =
    let (seen, newState) = processPulseP2 (createQueue (("Button", Low), "broadcaster")) cycleState cyclePaths endNode in
        if seen then n
        else processPulsesTillEnd (n+1) newState cyclePaths endNode

-- Returns when it sees the endstate mentioned has outputted a low
processPulseP2 :: Queue Instruction -> State -> Paths -> String -> (Bool, State)
processPulseP2 q state paths endNode =
    if isQueueEmpty q then (False, state)
    else
        let (((oldModule, sig), newModuleName), updatedQueue) = pop q in
            if oldModule == endNode && sig == Low && elem newModuleName (getFourConjunctions paths) then (True, state)
            else if sig == None then processPulseP2 updatedQueue state paths endNode
            else
                case Map.lookup newModuleName state of
                    Just newModule ->
                        let (updatedNewModule, outputtedSignal) = applyModule oldModule sig newModule in
                        let pushedUpdatedQueue = pushAll updatedQueue (affectedModules paths newModuleName outputtedSignal) in
                        processPulseP2 pushedUpdatedQueue (Map.insert newModuleName updatedNewModule state) paths endNode
                    Nothing -> processPulseP2 updatedQueue state paths endNode
