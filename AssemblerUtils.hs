module AssemblerUtils (
  intToSignExt,
  offsetCalculate,
  immediateValueToInt

) where

import Numeric (showHex, showIntAtBase)
import Data.Bits 
import Data.Char (intToDigit)
import Data.Word 

immediateValueToInt :: String -> Int
immediateValueToInt [] = 0
immediateValueToInt (x:xs) 
  | x == '#' = read xs
  | otherwise = 0


offsetCalculate :: Int -> Int -> Int
offsetCalculate labelAdd currentAdd =
    labelAdd - currentAdd - 1


intToSignExt :: Int -> Int -> String
intToSignExt value toSize
  | value < 0 = bitExtension (toStringBinary (complement value + 1)) toSize '1'
  | otherwise  = bitExtension (toStringBinary value) toSize '0'


repeateChar :: Char -> Int -> String
repeateChar myChar ammount 
  | ammount <= 0 = ""
  | otherwise  = myChar : repeateChar myChar (ammount - 1)

bitExtension :: String -> Int -> Char -> String
bitExtension value toSize charToAdd
  | toAdd < 0 = error "Wrong extension size"
  | otherwise = repeateChar charToAdd toAdd  ++ value
  where toAdd = toSize - length value


toStringBinary :: Int -> String
toStringBinary x = showIntAtBase 2 intToDigit x ""
