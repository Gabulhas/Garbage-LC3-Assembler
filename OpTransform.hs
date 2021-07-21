module OpTransform(
    opTransform
) where
import AssemblerUtils (intToSignExt, offsetCalculate, immediateValueToInt, labelOrOffsetToBinary, invalidOperands, trapValueToBin)
import LookupTables (getOpcode, getRegister, getDirective, justGetRegister, justGetOpcode, justGetDirective)
import Data.Bits 
import qualified Data.Map as M

-- The LC-3 ISA, from "Appendix A", available at:
-- http://highered.mheducation.com/sites/0072467509/student_view0/appendices_a__b__c__d____e.html
--
--       15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
-- ADD  | 0   0   0   1 |     DR    |    SR1    | 0 | 0   0 |    SR2    |
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
-- ADD  | 0   0   0   1 |     DR    |    SR1    | 1 |       imm5        |
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
-- AND  | 0   1   0   1 |     DR    |    SR1    | 0 | 0   0 |    SR2    |
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
-- AND  | 0   1   0   1 |     DR    |    SR1    | 1 |       imm5        |
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
-- BR   | 0   0   0   0 | n | z | p |             PCoffset9             |
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
-- JMP  | 1   1   0   0 | 0   0   0 |   BaseR   | 0   0   0   0   0   0 |
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
-- JSR  | 0   1   0   0 | 1 |               PCoffset11                  |
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
-- JSRR | 0   1   0   0 | 0 | 0   0 |   BaseR   | 0   0   0   0   0   0 |
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
-- LD   | 0   0   1   0 |     DR    |             PCoffset9             |
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
-- LDI  | 1   0   1   0 |     DR    |             PCoffset9             |
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
-- LDR  | 0   1   1   0 |     DR    |   BaseR   |        offset6        |
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
-- LEA  | 1   1   1   0 |     DR    |             PCoffset9             |
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
-- NOT  | 1   0   0   1 |     DR    |     SR    | 1   1   1   1   1   1 |
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
-- RET  | 1   1   0   0 | 0   0   0 | 1   1   1 | 0   0   0   0   0   0 |
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
-- RTI  | 1   0   0   0 | 0   0   0   0   0   0   0   0   0   0   0   0 |
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
-- ST   | 0   0   1   1 |     SR    |             PCoffset9             |
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
-- STI  | 1   0   1   1 |     SR    |             PCoffset9             |
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
-- STR  | 0   1   1   1 |     SR    |   BaseR   |        offset6        |
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
-- TRAP | 1   1   1   1 | 0   0   0   0 |          trapvect8            |
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
-- N/A  | 1   1   0   1 |                                               |
--      +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+



opTransform :: [String] -> M.Map String Int -> Int -> String
opTransform [] _ _ = ""
opTransform (opcode:operands) symbolsMap currentAddress
  | opcode ==  "ADD" =   addop operands
  | opcode ==  "AND" =   andop operands 
  | opcode ==  "BR" = brop "111" (last operands) symbolsMap currentAddress
  | opcode ==  "BRN" = brop "100" (last operands) symbolsMap currentAddress
  | opcode ==  "BRNP" = brop "101" (last operands) symbolsMap currentAddress
  | opcode ==  "BRNZ" = brop "110" (last operands) symbolsMap currentAddress
  | opcode ==  "BRNZP" = brop "111" (last operands) symbolsMap currentAddress
  | opcode ==  "BRP" = brop "001" (last operands) symbolsMap currentAddress
  | opcode ==  "BRZ" = brop "010" (last operands) symbolsMap currentAddress
  | opcode ==  "BRZP" = brop "011" (last operands) symbolsMap currentAddress
  | opcode ==  "JMP" =   jmpop (head operands) 
  | opcode ==  "RET" =   "1100000111000000"
  | opcode ==  "JSR" =  jsrop (head operands) symbolsMap currentAddress
  | opcode ==  "JSRR" =  jsrrop (head operands)
  | opcode ==  "LD" =    ldop operands symbolsMap currentAddress
  | opcode ==  "LDI" =   ldiop operands symbolsMap currentAddress
  | opcode ==  "LDR" =   ldrop operands symbolsMap currentAddress
  | opcode ==  "LEA" =   leaop operands symbolsMap currentAddress
  | opcode ==  "NOT" =   notop operands 
  | opcode ==  "RTI" =   "1000000000000000"
  | opcode ==  "ST" =    stop operands symbolsMap currentAddress
  | opcode ==  "STI" =   stiop operands symbolsMap currentAddress
  | opcode ==  "STR" =   strop operands symbolsMap currentAddress
  | opcode ==  "TRAP" =  trapop (head operands)
  | opcode ==  "GETC" =  trapop "x20"
  | opcode ==  "OUT" =   trapop "x21"
  | opcode ==  "PUTS" =  trapop "x22"
  | opcode ==  "IN" =    trapop "x23"
  | opcode ==  "PUTSP" = trapop "x24"
  | opcode ==  "HALT" =  trapop "x25"
  | opcode ==  "NONE" =  "1101"
  | otherwise = error ("Invalid opcode '"  ++ opcode ++ "'")


addop :: [String] -> String
addop []  = ""
addop operands 
  -- If it's using the value from a register
  | Just resultRegister <- getRegister lastOperand = do
      firstPart ++ "000" ++ resultRegister
  | otherwise = do
      firstPart ++ "1" ++  intToSignExt (immediateValueToInt lastOperand) 5
  where lastOperand = last operands
        (dr:sr1:_) = operands 
        firstPart = "0001" ++ justGetRegister dr ++ justGetRegister sr1

andop :: [String] -> String
andop [] = ""
andop operands 
  -- If it's using the value from a register
  | Just resultRegister <- getRegister lastOperand = do
      firstPart ++ "000" ++ resultRegister
  | otherwise = do
      firstPart ++ "1" ++ intToSignExt (immediateValueToInt lastOperand) 5
  where lastOperand = last operands
        (dr:sr1:_) = operands 
        firstPart = "0101" ++ justGetRegister dr ++ justGetRegister sr1




brop :: String -> String -> M.Map String Int -> Int -> String
brop brType labelOrOffset symbolsMap currentAddress =
    "0000" ++ brType ++ labelOrOffsetToBinary labelOrOffset  symbolsMap currentAddress 9

jmpop :: String -> String
jmpop reg = "1100000" ++ justGetRegister reg ++ "000000"


jsrop :: String -> M.Map String Int -> Int -> String
jsrop labelOrOffset symbolsMap currentAddress =
    "01001" ++ labelOrOffsetToBinary labelOrOffset symbolsMap currentAddress 11

jsrrop :: String -> String
jsrrop reg = "0100000" ++ justGetRegister reg ++ "000000"
  
ldop :: [String] -> M.Map String Int -> Int -> String
ldop (dr:labelOrOffset:_) symbolsMap currentAddress =
    "0010" ++ justGetRegister dr ++ labelOrOffsetToBinary labelOrOffset symbolsMap currentAddress 9
ldop operands _ _ = error (invalidOperands operands)

ldiop :: [String] -> M.Map String Int -> Int -> String
ldiop (dr:labelOrOffset:_) symbolsMap currentAddress =
    "1010" ++ justGetRegister dr ++ labelOrOffsetToBinary labelOrOffset symbolsMap currentAddress 9
ldiop operands _ _ = error (invalidOperands operands)

ldrop :: [String] -> M.Map String Int -> Int -> String
ldrop (dr:baser:labelOrOffset:_) symbolsMap currentAddress =
    "0110" ++ justGetRegister dr ++ justGetRegister baser ++ labelOrOffsetToBinary labelOrOffset symbolsMap currentAddress 6
ldrop operands _ _ = error (invalidOperands operands)

leaop :: [String] -> M.Map String Int -> Int -> String
leaop (dr:labelOrOffset:_) symbolsMap currentAddress =
    "1110" ++ justGetRegister dr ++ labelOrOffsetToBinary labelOrOffset symbolsMap currentAddress 9
leaop operands _ _ = error (invalidOperands operands)


notop :: [String] -> String
notop (dr:sr:_) =
    "1001" ++ justGetRegister dr ++ justGetRegister sr ++ "111111"
notop operands = error (invalidOperands operands)

stop :: [String] -> M.Map String Int -> Int -> String
stop (sr:labelOrOffset:_) symbolsMap currentAddress =
    "0011" ++ justGetRegister sr ++ labelOrOffsetToBinary labelOrOffset symbolsMap currentAddress 9
stop operands _ _ = error (invalidOperands operands)


stiop :: [String] -> M.Map String Int -> Int -> String
stiop (sr:labelOrOffset:_) symbolsMap currentAddress =
    "1011" ++ justGetRegister sr ++ labelOrOffsetToBinary labelOrOffset symbolsMap currentAddress 9
stiop operands _ _ = error (invalidOperands operands)

strop :: [String] -> M.Map String Int -> Int -> String
strop (sr:baser:labelOrOffset:_) symbolsMap currentAddress =
    "0111" ++ justGetRegister sr ++ justGetRegister baser ++ labelOrOffsetToBinary labelOrOffset symbolsMap currentAddress 6
strop operands _ _ = error (invalidOperands operands)


trapop :: String -> String
trapop val =
    "11110000" ++ trapValueToBin val
