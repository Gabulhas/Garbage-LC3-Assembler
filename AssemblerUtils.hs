module AssemblerUtils (
  intToSignExt,
  offsetCalculate,
  immediateValueToInt,
  labelOrOffsetToBinary,
  invalidOperands,
  trapValueToBin,
  dirValueToBin,
  hexToInt,
  bitExtension,
  toStringBinary,
  literalValueToBinExtended
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
offsetCalculate labelAdd currentAdd = do
    if labelAdd > currentAdd then labelAdd - currentAdd - 1
                             else  (-1) * ( currentAdd + 1 - labelAdd)


labelOrOffsetToBinary :: String -> M.Map String Int -> Int -> Int -> String
labelOrOffsetToBinary val symbolsMap currentAddress extension
  | currentHead == '#' || currentHead == 'x' || currentHead == 'X' = literalValueToBinExtended val extension
  | Just resultAddress <- M.lookup val symbolsMap =  intToSignExt (offsetCalculate resultAddress currentAddress) extension
  | otherwise = error "Not an offset nor a label"
  where currentHead = head val

intToSignExt :: Int -> Int -> String
intToSignExt value toSize
  -- wtf is this????????
  -- so, to SignExtend with the size X a negative value, you just need to do 2^X + value
  -- spent hours on this bug (;¬_¬)
  | value < 0 = toStringBinary (2^toSize +  value)
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


invalidOperands :: [String] -> String
invalidOperands operands = "Invalid operands " ++ unwords operands


trapValueToBin :: String -> String 
trapValueToBin val = literalValueToBinExtended val 8


dirValueToBin :: String -> String
dirValueToBin val = literalValueToBinExtended val 16

literalValueToBinExtended :: String -> Int ->String
literalValueToBinExtended val extension
  | currentHead == '#' = intToSignExt  (immediateValueToInt val) extension
  | currentHead == 'x' || currentHead == 'X' = intToSignExt (hexToInt (tail val)) extension
  | otherwise = error ("Not an immediate value nor an hexadecimal value"++ val)
  where currentHead = head val


hexToInt :: String -> Int
hexToInt "" = error "Invalid Hex Value"
hexToInt val = hexToIntAux val 0



hexToIntAux :: String -> Int -> Int
hexToIntAux [] accum = accum
hexToIntAux xs accum
    = foldl (\ accum x -> accum * 16 + hexDigitToInt x) accum xs



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


