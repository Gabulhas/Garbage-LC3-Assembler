module AssemblerUtils (
  intToSignExt,
  offsetCalculate,
  immediateValueToInt,
  labelOrOffsetToBinary,
  invalidOperands,
  trapValueToBin

) where

import Numeric (showHex, showIntAtBase)
import Data.Bits 
import Data.Char (intToDigit, digitToInt)
import Data.Word 
import qualified Data.Map as M
import Data.List

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



labelOrOffsetToBinary :: String -> M.Map String Int -> Int -> Int -> String
labelOrOffsetToBinary val symbolsMap currentAddress extension
  | head val == '#' = intToSignExt  (immediateValueToInt val) extension
  | Just resultAddress <- M.lookup val symbolsMap =  intToSignExt (offsetCalculate resultAddress currentAddress) extension
  | otherwise = error "Not an offset nor a label"

invalidOperands :: [String] -> String
invalidOperands operands = "Invalid operands " ++ unwords operands


trapValueToBin :: String -> String 
trapValueToBin val 
  | head val == '#' = intToSignExt  (immediateValueToInt val) 8
  | head val == 'x' = concatMap hexCharToBin (tail val)
  | otherwise = error "Not an immediate value nor an hexadecimal value"



hexCharToBin :: Char -> String
hexCharToBin value =
    intToSignExt (hexDigitToInt value) 4
    

hexDigitToInt :: Char -> Int
hexDigitToInt x 
  | x == 'A' = 10
  | x == 'B' = 11
  | x == 'C' = 12
  | x == 'D' = 13
  | x == 'E' = 14
  | x == 'F' = 15
  | otherwise = digitToInt  x :: Int
