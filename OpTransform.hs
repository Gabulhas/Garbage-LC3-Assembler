module OpTransform(
    assembleLine
) where
import AssemblerUtils (intToSignExt, offsetCalculate, immediateValueToInt)
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



assembleLine :: [String] -> M.Map String Int -> Int -> String
assembleLine [] _ _ = ""
assembleLine (opcode:operands) symbolsMap currentAddress
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
--  | opcode ==  "JSRR" =  jsrrop operands symbolsMap currentAddress
--  | opcode ==  "LD" =    ldop operands symbolsMap currentAddress
--  | opcode ==  "LDI" =   ldiop operands symbolsMap currentAddress
--  | opcode ==  "LDR" =   ldrop operands symbolsMap currentAddress
--  | opcode ==  "LEA" =   leaop operands symbolsMap currentAddress
--  | opcode ==  "NOT" =   notop operands symbolsMap currentAddress
  | opcode ==  "RTI" =   "1000000000000000"
--  | opcode ==  "ST" =    stop operands symbolsMap currentAddress
--  | opcode ==  "STI" =   stiop operands symbolsMap currentAddress
--  | opcode ==  "STR" =   strop operands symbolsMap currentAddress
--  | opcode ==  "TRAP" =  trapop operands symbolsMap currentAddress
--  | opcode ==  "GETC" =  getcop operands symbolsMap currentAddress
--  | opcode ==  "OUT" =   outop operands symbolsMap currentAddress
--  | opcode ==  "PUTS" =  putsop operands symbolsMap currentAddress
--  | opcode ==  "IN" =    inop operands symbolsMap currentAddress
--  | opcode ==  "PUTSP" = putspop operands symbolsMap currentAddress
--  | opcode ==  "HALT" =  haltop operands symbolsMap currentAddress
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
brop brType offsetOrLabel symbolsMap currentAddress 
  | head offsetOrLabel == '#' = firstPart ++ intToSignExt  (immediateValueToInt offsetOrLabel) 9
  | Just resultAddress <- M.lookup offsetOrLabel symbolsMap = firstPart ++  intToSignExt (offsetCalculate resultAddress currentAddress) 9
 
  | otherwise = error "Not an offset nor a label"
   where firstPart = "0000" ++ brType

jmpop :: String -> String
jmpop reg = "1100000" ++ justGetRegister reg ++ "000000"



-- jsrrop :: [String] -> M.Map String Int -> Int -> String
-- ldop :: [String] -> M.Map String Int -> Int -> String
-- ldiop :: [String] -> M.Map String Int -> Int -> String
-- ldrop :: [String] -> M.Map String Int -> Int -> String
-- leaop :: [String] -> M.Map String Int -> Int -> String
-- notop :: [String] -> M.Map String Int -> Int -> String
-- stop :: [String] -> M.Map String Int -> Int -> String
-- stiop :: [String] -> M.Map String Int -> Int -> String
-- strop :: [String] -> M.Map String Int -> Int -> String
-- trapop :: [String] -> M.Map String Int -> Int -> String
-- getcop :: [String] -> M.Map String Int -> Int -> String
-- outop :: [String] -> M.Map String Int -> Int -> String
-- putsop :: [String] -> M.Map String Int -> Int -> String
-- inop :: [String] -> M.Map String Int -> Int -> String
-- putspop :: [String] -> M.Map String Int -> Int -> String
-- haltop :: [String] -> M.Map String Int -> Int -> String

