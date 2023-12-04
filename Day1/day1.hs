{-# LANGUAGE OverloadedStrings #-}

import Data.String
import Data.Char
import Data.Text(pack, unpack, replace)

main :: IO ()
main = do
  contents <- readFile "input"
  putStrLn (show (sum (processLines(lines contents))))

replaceNumbers :: String -> String
replaceNumbers = unpack . replace "one" "o1ne" .
                          replace "two" "t2o".
                          replace "three" "t3e".
                          replace "four" "f4r".
                          replace "five" "f5e".
                          replace "six" "s6x".
                          replace "seven" "s7n".
                          replace "eight" "e8t".
                          replace "nine" "n9n". pack

processLines :: [String] -> [Int]
processLines [] = []
processLines (x:xs) = (processLine(replaceNumbers x)) : (processLines xs)

processLine :: String -> Int
processLine x = 
    let nums = numbers(x) in
        let beginning = head(nums) in 
            let end = last(nums) in 
    read ([beginning] ++ [end]) :: Int

numbers :: String -> [Char]
numbers "" = []
numbers (l:ls) = if ord l >= 48 && ord l <= 57
    then (l : (numbers ls))
    else (numbers ls)