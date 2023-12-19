{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.List.Split
import qualified Data.Map as Map

-- Value of each attribute of a part
data Part = Part
    {
        x :: Int,
        m :: Int,
        a :: Int,
        s :: Int
    } deriving (Eq, Show)

-- An alias for the State
type State = String

-- Representation of the conditionals. The attribute, whether its a < or > and the number
data ConditionalRule = ConditionalRule
    {
        value :: Char,
        gt :: Bool, -- If the rule is greater than, if false then it's less than
        num :: Int
    } deriving (Eq, Show)

-- Either there's a conditional to a state or it has no rules and goes directly to a state (only if exhausted every conditional)
data Rule = Rules (State, ConditionalRule) | NoRules State deriving (Eq, Show)

-- Range for each value exclusive on both ends
data PartRange = PartRange
    {
        xRange :: (Int, Int),
        mRange :: (Int, Int),
        aRange :: (Int, Int),
        sRange :: (Int, Int)
    } deriving (Eq, Show)

-- A tree of states as vertices and ConditionalRules as edges such that to get to another state, you have to have the conditional be true.
data Tree = Leaf State | Branch State ConditionalRule Tree ConditionalRule Tree deriving (Eq, Show)

main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day19/input"
    let rulesAndVals = splitOn [""] (lines contents) -- Split the rules and values sections
    let vals = map parseVal (rulesAndVals !! 1)
    let rules = Map.fromList (map parseStateWithRules (head rulesAndVals))
    let endStates = map (findEndState rules "in") vals
    let ratings = map (sumOfPart . fst) (filter (\n -> snd n == "A") (zip vals endStates))
    print (sum ratings)

    -- Part 2 
    -- We split up the states so that it's either a conditional or a NoRule so that we can convert it to a binary tree
    -- The tree will then have a conditional on each branch with the left one being the real one and the right being the opposite of it
    let treeifiedRules = Map.fromList (splitRules (map parseStateWithRules (head rulesAndVals)))
    -- Search the tree for every path to an accepted node from "in" and only store the ConditionalRules taken to get there
    let allAPaths = filter (/= []) (findAs (createTree treeifiedRules "in") [])
    -- For each rule set, apply it to a PartRange to get the range and figure out how many Parts can exist in that range and sum them up
    print(sum(map (partRangeToPossibleCombos . getPartRange (PartRange (0, 4001) (0, 4001) (0, 4001) (0, 4001))) allAPaths))

-- Gets the corresponding value for a Part; just for part 1
sumOfPart :: Part -> Int
sumOfPart (Part x m a s) = x + m + a + s

-- Parses a line to get the list of rules mentioned
parseStateWithRules :: String -> (State, [Rule])
parseStateWithRules s = do
    let statePlusRule = splitOn "{" s
    let setOfRules = init (last statePlusRule)
    (head statePlusRule, map parseRule (splitOn "," setOfRules))

-- Given a string like "x<2152:R" returns ("R", ConditionalRule 'x' False 2152)
parseRule :: String -> Rule
parseRule s = do
    let splitRule = splitOn ":" s
    if length splitRule == 2 then
        let rule = head splitRule in
        Rules (last splitRule, ConditionalRule (head rule) ((rule !! 1) == '>') (read (drop 2 rule) :: Int))
    else
        NoRules (head splitRule)

-- Parses a Part value from the file
parseVal :: String -> Part
parseVal s =
    let vals = splitOn "," (init (tail s)) in
    let parsedVals = map ((read :: String -> Int) . last . splitOn "=") vals in
        Part (head parsedVals) (parsedVals !! 1) (parsedVals !! 2) (parsedVals !! 3)

-- Given a Part and a set of rules, returns the resulting state. 
applyRules :: Part -> [Rule] -> State
applyRules _ [NoRules s] = s
applyRules (Part x m a s) (Rules (state, ConditionalRule val gt num) : rules) =
    case val of
        'x' ->
            if gt then
                if x > num then state else applyRules (Part x m a s) rules
            else
                if x < num then state else applyRules (Part x m a s) rules
        'm' ->
            if gt then
                if m > num then state else applyRules (Part x m a s) rules
            else
                if m < num then state else applyRules (Part x m a s) rules
        'a' ->
            if gt then
                if a > num then state else applyRules (Part x m a s) rules
            else
                if a < num then state else applyRules (Part x m a s) rules
        's' ->
            if gt then
                if s > num then state else applyRules (Part x m a s) rules
            else
                if s < num then state else applyRules (Part x m a s) rules

-- Given a Map of States -> Rules, the initial state and the part, gets the end state of A vs R
findEndState :: Map.Map State [Rule] -> State -> Part -> State
findEndState m state p =
    case Map.lookup state m of
        Just r ->
            let newState = applyRules p r in
                if newState == "A" || newState == "R" then newState else findEndState m newState p
        Nothing -> error "Shouldnt happen due to precondition of problem (it would be weird if it linked to a state that doesnt exist)"

-- Part 2

-- Splits up a set of rules such that the [Rule] array only has 1 conditional and a NoRule; This way it can be parsed as "if this else this" rather than a bunch of else ifs
splitRules :: [(State, [Rule])] -> [(State, [Rule])]
splitRules                 [] = []
splitRules ((state, r:rs):rest) =
    if length (r:rs) == 2 then (state, r:rs) : splitRules rest
    else (state, [r, NoRules (state ++ "1")]) : splitRules ((state ++ "1", rs):rest)

-- Given a Map of States to the 2 length rule set and the starting state, creates a tree of it. 
createTree :: Map.Map State [Rule] -> State -> Tree
createTree rules state =
    case Map.lookup state rules of
        Just [Rules (newState1, ConditionalRule v gt num), NoRules newState2] ->
            if gt then
                Branch state (ConditionalRule v True num) (createTree rules newState1) (ConditionalRule v False (num + 1)) (createTree rules newState2) -- Set of 2 rules, create branch
            else
                Branch state (ConditionalRule v False num) (createTree rules newState1) (ConditionalRule v True (num - 1)) (createTree rules newState2) -- Set of 2 rules, create branch
        Nothing -> Leaf state

-- Given a tree of the rules gets all paths that lead to an acceptance; returns all the Conditionals in the path
findAs :: Tree -> [ConditionalRule] -> [[ConditionalRule]]
findAs (Leaf "R") pathSoFar = [[]]
findAs (Leaf "A") pathSoFar = [pathSoFar]
findAs (Branch s leftRule leftTree rightRule rightTree) pathSoFar =
    findAs leftTree (pathSoFar ++ [leftRule]) ++ findAs rightTree (pathSoFar ++ [rightRule])

-- Given a PartRange, applies a list of rules to them to get the final PartRange
getPartRange :: PartRange -> [ConditionalRule] -> PartRange
getPartRange                      p                                    [] = p
getPartRange (PartRange xr mr ar sr) ((ConditionalRule val gt num):rules) =
    case val of
        'x' ->
            if gt then
                getPartRange (PartRange (fst xr `max` num, snd xr) mr ar sr) rules
            else
                getPartRange (PartRange (fst xr, snd xr `min` num) mr ar sr) rules
        'm' ->
            if gt then
                getPartRange (PartRange xr (fst mr `max` num, snd mr) ar sr) rules
            else
                getPartRange (PartRange xr (fst mr, snd mr `min` num) ar sr) rules
        'a' ->
            if gt then
                getPartRange (PartRange xr mr (fst ar `max` num, snd ar) sr) rules
            else
                getPartRange (PartRange xr mr (fst ar, snd ar `min` num) sr) rules
        's' ->
            if gt then
                getPartRange (PartRange xr mr ar (fst sr `max` num, snd sr)) rules
            else
                getPartRange (PartRange xr mr ar (fst sr, snd sr `min` num)) rules

-- Given a part range, find out how many parts can exist in this range
partRangeToPossibleCombos :: PartRange -> Int
partRangeToPossibleCombos (PartRange xr mr ar sr) =
    (snd xr - fst xr - 1) * (snd mr - fst mr - 1) * (snd ar - fst ar - 1) * (snd sr - fst sr - 1)
