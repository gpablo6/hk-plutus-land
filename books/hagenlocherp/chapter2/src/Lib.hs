module Lib where

type Alphabet = [Char]

lowerAlphabet :: Alphabet
lowerAlphabet = ['a' .. 'z']

upperAlphabet :: Alphabet
upperAlphabet = ['A' .. 'Z']

digits :: Alphabet
digits = ['0' .. '9']

isUpper :: Char -> Bool
isUpper char = char `elem` upperAlphabet

isLower :: Char -> Bool
isLower char = char `elem` lowerAlphabet

isDigit :: Char -> Bool
isDigit char = char `elem` digits

isMisc :: Char -> Bool
isMisc char = char `notElem` lowerAlphabet ++ upperAlphabet ++ digits

listLength [] = 0
listLength (x : xs) = 1 + listLength xs

indexOf :: Char -> Alphabet -> Int
indexOf ch [] = undefined
indexOf ch (x : xs) = 
    if x == ch
        then 0
    else 1 + indexOf ch xs

indexValue :: Int -> Alphabet -> Char
indexValue _ [] = undefined
indexValue 0 (x : xs) = x
indexValue ind xs 
    | ind < 0 = undefined
    | ind > listLength xs = undefined
indexValue ind (x : xs) = indexValue (ind - 1) xs

upperRot :: Int -> Char -> Char
upperRot n ch = indexValue ((indexOf ch upperAlphabet + n) `mod` 26) upperAlphabet

lowerRot :: Int -> Char -> Char
lowerRot n ch = indexValue ((indexOf ch lowerAlphabet + n) `mod` 26) lowerAlphabet