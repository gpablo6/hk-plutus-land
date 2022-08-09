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

alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot alphabet n ch =
    indexValue ((indexOf ch alphabet + n) `mod` 26) alphabet

upperRot :: Int -> Char -> Char
upperRot n ch = alphabetRot upperAlphabet n ch

lowerRot :: Int -> Char -> Char
lowerRot n ch = alphabetRot lowerAlphabet n ch

rotChar :: Int -> Char -> Char
rotChar n ch
    | isUpper ch = upperRot n ch
    | isLower ch = lowerRot n ch
    | otherwise = ch

caesar :: Int -> String -> String
caesar n [] = []
caesar n (x: xs) = rotChar n x : caesar n xs