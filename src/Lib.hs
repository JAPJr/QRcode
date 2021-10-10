{--
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
--}

module Lib where

import Data.Char
import Data.Maybe

-- GENERAL

type Binary = String

decToBin :: Int -> Binary
decToBin n = addBits (n `div` 2)  (getLowBit n : "")
  where getLowBit num = intToDigit (num `mod` 2)
        addBits num bin
          |num == 0 = bin
          |otherwise = addBits (num `div` 2) (getLowBit num : bin)

paddToN :: Int -> Binary -> Binary
paddToN n bin = replicate (n - length bin) '0' ++ bin

paddRightN n bin = bin ++ replicate n '0'

data Mode = Numeric | Alpha | Byte | Kanji deriving Eq
data ECLevel = L | M | Q | H deriving Eq


modeIndicator mode = case mode of
  Numeric -> "0001"
  Alpha   -> "0010"
  Byte    -> "0100"
  Kanji   -> "1000"

charCountSize :: Int -> Mode -> Int
charCountSize ver mode
  |ver < 10  = fromJust $ lookup mode smallCountSize 
  |ver < 27  = fromJust $ lookup mode mediumCountSize
  |otherwise = fromJust $ lookup mode largeCountSize
    where smallCountSize = [(Numeric, 10), (Alpha, 9), (Byte, 8), (Kanji, 8)]
          mediumCountSize = [(Numeric, 12), (Alpha, 11), (Byte, 16), (Kanji, 10)]
          largeCountSize = [(Numeric, 14), (Alpha, 13), (Byte, 16), (Kanji, 12) ] 

nDataWords ver ecLevel = fromJust $ lookup ecLevel $ fromJust $ lookup ver nWordsTable
  where nWordsTable = [(1, [(L, 19), (M, 16), (Q, 13), (H, 9)]), (2, [(L, 34), (M, 28), (Q, 22), (H, 16)]), (3, [(L,55), (M, 44), (Q, 34), (H, 26)]), (4, [(L, 80), (M, 64), (Q, 48), (H, 36)]) ]

-- BYTE MODE

charTo8Bits :: Char -> Binary
charTo8Bits = paddToN 8 . decToBin . fromEnum

encodeText :: String -> Binary
encodeText txt = foldr addCharCodes "" txt
  where addCharCodes c code = charTo8Bits c ++ code


-- ALPHANUMERIC MODE

alphaVal :: Char -> Int
alphaVal char = fromJust $ lookup char alphaNumericTable
  where alphaNumericTable = zip (['0' .. '9'] ++ ['A' .. 'Z'] ++ [' ', '$', '%', '*', '+', '-', '.', '/', ':']) [0 .. 44] 

alphaCodeToBin :: [Int] -> Binary
alphaCodeToBin [] = ""
alphaCodeToBin (a : []) = paddToN 6 $ decToBin a
alphaCodeToBin (a1 : a2 : as) = (paddToN 11 $ decToBin (a1 * 45 + a2) ) ++ alphaCodeToBin as


alphaEncodeText text ver ecLevel = paddRightN nPadding unpaddedCode 
  where modeInd = modeIndicator Alpha
        charCount = paddToN 9 $ decToBin $ length text
        dataCode = alphaCodeToBin $ map alphaVal text 
        unpaddedCode = modeInd ++ charCount ++ dataCode
        codeLength = length unpaddedCode
        diff = 8 * nDataWords ver ecLevel - codeLength 
        nPadding
          |diff < 5  = diff
          |otherwise = 4               
