module AssemblerUtils (
  imm5 
) where

import Numeric (showHex, showIntAtBase)




imm5 :: String -> String
imm5 "" = error "Not a Number"
imm5 ('#':value) 
  | val < (-16) || val > 15 = error "Immediate value not in range"
  | val < 0 =  "1" ++ addTrailZeros 4 (num2bin ((-1) * val))
  | otherwise  = addTrailZeros 5 $ num2bin val
  where valHead = head value
        val = read value

imm5 _ = error "Imediate value not starting with a #"



num2bin :: (Integral a, Show a) => a -> String
num2bin 0 = "0"
num2bin 1 = "1"
num2bin n
    | n < 0         = error "Negative number"
    | otherwise     = num2bin (n `div` 2) ++ show (n `mod` 2)


-- Adds trailing zeros to binary value: Example: addTrailZeros 5 "001" -> "00001"
addTrailZeros :: Int -> String -> String
addTrailZeros wanted current 
  | toAdd < 0 = error "Negative number of trailing zeros"
  | otherwise = repeateChar '0' toAdd ++ current
  where toAdd = wanted - length current
    

repeateChar myChar ammount 
  | ammount <= 0 = ""
  | otherwise  = myChar : repeateChar myChar (ammount - 1)

