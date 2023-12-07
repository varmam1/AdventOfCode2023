{-# LANGUAGE OverloadedStrings #-}
import Data.List.Split
import Data.List
import Data.Text(pack, unpack, replace)
import Debug.Trace
import Data.List (sortBy)
import Data.Function (on)
import Data.Map (fromListWith, toList)

-- Have a specific Joker for p2 which can be parsed with a different function; shouldn't be both Js and Jokers in the same part
data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | T | J | Q | K | A deriving (Eq,Ord,Enum,Bounded,Show,Read)

-- Actual results with Bets
data HandDescriptor = HighCard([Card], Int)
    | OnePair([Card], Int) 
    | TwoPair([Card], Int) 
    | ThreeOfAKind([Card], Int) 
    | FullHouse([Card], Int) 
    | FourOfAKind([Card], Int) 
    | FiveOfAKind([Card], Int) deriving (Eq,Ord,Show,Read)

main :: IO()
main = do
    -- Part 1
    contents <- readFile "Day7/input"
    let sortedHands = sort(parseLinesAsHands (lines contents) getHandResultP1)
    let ans1 = sum(zipWith (*) [1..] (getAllOrderedBets sortedHands))
    putStrLn (show(ans1))

    -- Part 2
    let sortedHands2 = sort(parseLinesAsHands (lines contents) getHandResultP2)
    let ans2 = sum(zipWith (*) [1..] (getAllOrderedBets sortedHands2))
    putStrLn (show(ans2))

parseLinesAsHands :: [String] -> (String -> Int -> HandDescriptor) -> [HandDescriptor]
parseLinesAsHands [] _ = []
parseLinesAsHands (x:xs) handResultGetter = (parseLine x handResultGetter) : (parseLinesAsHands xs handResultGetter)

parseLine :: String -> (String -> Int -> HandDescriptor) -> HandDescriptor
parseLine l handResultGetter = let splt = (splitOn " " l) in handResultGetter (head(splt)) (read (last(splt)) :: Int)

getHandResultP1 :: String -> Int -> HandDescriptor
getHandResultP1 hand bet = do 
    let cards = parseStrToCard hand 
    let freq = frequency cards
    parseFreqList freq cards bet

-- Parse the freq list for a hand to get the type of Hand it is
parseFreqList :: [(Card, Int)] -> [Card] -> Int -> HandDescriptor
parseFreqList freq cards bet
    | (length(freq) == 1) = FiveOfAKind(cards, bet)
    | (length(freq) == 2) && (checkExists freq 4) = FourOfAKind(cards, bet)
    | (length(freq) == 2) = FullHouse(cards, bet)
    | (length(freq) == 3) && (checkExists freq 3) = ThreeOfAKind(cards, bet)
    | (length(freq) == 3) = TwoPair(cards, bet)
    | (length(freq) == 4) = OnePair(cards, bet)
    | otherwise = HighCard(cards, bet)

-- There are 2 cases where the length of the frequencies won't be enough; 2 and 3. 
-- For length of 2, if there is a 4 in the frequencies then its a four pair, else full house
-- For length of 3, if there's a 3 then its a 3 of a kind else 2 pair
checkExists :: [(a, Int)] -> Int -> Bool
checkExists [] _ = False
checkExists ((a, x):xs) val = (x == val) || (checkExists (xs) val)

-- Get freq of each value in a list of cards
frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

-- Part 2 Functions

getHandResultP2 :: String -> Int -> HandDescriptor
getHandResultP2 hand bet = do 
    -- Parse using P2 parse function so we have Js as "Joker" in code
    let cards = parseStrToCardP2 hand 
    let freq = frequency cards
    -- Remove all of the Js from the freq list and get how many there were 
    let freqNoJs = removeJsAndGetNumber freq
    let numJs = snd(freqNoJs)
    let sortedFreq = reverse((sortBy (compare `on` sec)) (fst(freqNoJs))) 
    -- Update the list by adding the Js to the highest freq value
    let updatedFreqList = updateFreqList sortedFreq numJs
    -- Do the same parsing as P1
    parseFreqList updatedFreqList cards bet

-- Remove all of the Js from the freq list (unless there are 5 of them) and then return how many to redistribute
removeJsAndGetNumber :: [(Card, Int)] -> ([(Card, Int)], Int)
removeJsAndGetNumber [] = ([], 0)
removeJsAndGetNumber ((Joker, 5):xs) = ((Joker, 5):xs, 0)
removeJsAndGetNumber ((Joker, x):xs) = (xs, x)
removeJsAndGetNumber (x:xs) = let js = removeJsAndGetNumber xs in ((x : fst(js)), snd(js))

-- Update freq of first val with number of Js
updateFreqList :: [(Card, Int)] -> Int -> [(Card, Int)]
updateFreqList ((a, x):xs) val = ((a, x + val) : xs)

-- Helpers

-- Once the list is ordered, only get the bets since thats all we care about now
getAllOrderedBets :: [HandDescriptor] -> [Int]
getAllOrderedBets [] = []
getAllOrderedBets ((HighCard(_, x)):xs) = x : (getAllOrderedBets(xs))
getAllOrderedBets ((OnePair(_, x)):xs) = x : (getAllOrderedBets(xs))
getAllOrderedBets ((TwoPair(_, x)):xs) = x : (getAllOrderedBets(xs))
getAllOrderedBets ((ThreeOfAKind(_, x)):xs) = x : (getAllOrderedBets(xs))
getAllOrderedBets ((FullHouse(_, x)):xs) = x : (getAllOrderedBets(xs))
getAllOrderedBets ((FourOfAKind(_, x)):xs) = x : (getAllOrderedBets(xs))
getAllOrderedBets ((FiveOfAKind(_, x)):xs) = x : (getAllOrderedBets(xs))

-- Parsing Functions from String -> Card
parseStrToCard :: String -> [Card]
parseStrToCard "" = []
parseStrToCard (x:xs) = (parseCharToCard x) : (parseStrToCard xs)

parseCharToCard :: Char -> Card
parseCharToCard '2' = Two
parseCharToCard '3' = Three
parseCharToCard '4' = Four
parseCharToCard '5' = Five
parseCharToCard '6' = Six
parseCharToCard '7' = Seven
parseCharToCard '8' = Eight
parseCharToCard '9' = Nine
parseCharToCard 'T' = T
parseCharToCard 'J' = J
parseCharToCard 'Q' = Q
parseCharToCard 'K' = K
parseCharToCard 'A' = A

parseStrToCardP2 :: String -> [Card]
parseStrToCardP2 "" = []
parseStrToCardP2 (x:xs) = (parseCharToCardP2 x) : (parseStrToCardP2 xs)

parseCharToCardP2 :: Char -> Card
parseCharToCardP2 '2' = Two
parseCharToCardP2 '3' = Three
parseCharToCardP2 '4' = Four
parseCharToCardP2 '5' = Five
parseCharToCardP2 '6' = Six
parseCharToCardP2 '7' = Seven
parseCharToCardP2 '8' = Eight
parseCharToCardP2 '9' = Nine
parseCharToCardP2 'T' = T
parseCharToCardP2 'J' = Joker
parseCharToCardP2 'Q' = Q
parseCharToCardP2 'K' = K
parseCharToCardP2 'A' = A

-- Use for the sorting 
sec :: (a, b) -> b
sec (_, b) = b
