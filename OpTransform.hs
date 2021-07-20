module OpTransform(
    assembleLine
) where
import AssemblerUtils (imm5)
import LookupTables (getOpcode, getRegister, getDirective, justGetRegister, justGetOpcode, justGetDirective)
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



assembleLine :: [String] -> M.Map String Integer -> Integer -> String
assembleLine [] _ _ = ""
assembleLine (opcode:operands) symbolsMap currentAddress
  | opcode ==  "ADD" =   addop operands
  | opcode ==  "AND" =   andop operands 
--  | opcode ==  "BRN" =   brnop operands symbolsMap currentAddress
--  | opcode ==  "BRZ" =   brzop operands symbolsMap currentAddress
--  | opcode ==  "BRP" =   brpop operands symbolsMap currentAddress
--  | opcode ==  "BR" =    brop operands symbolsMap currentAddress
--  | opcode ==  "BRZP" =  brzpop operands symbolsMap currentAddress
--  | opcode ==  "BRNP" =  brnpop operands symbolsMap currentAddress
--  | opcode ==  "BRNZ" =  brnzop operands symbolsMap currentAddress
--  | opcode ==  "BRNZP" = brnzpop operands symbolsMap currentAddress
--  | opcode ==  "JMP" =   jmpop operands symbolsMap currentAddress
--  | opcode ==  "RET" =   "1100000111000000"
--  | opcode ==  "JSRR" =  jsrrop operands symbolsMap currentAddress
--  | opcode ==  "LD" =    ldop operands symbolsMap currentAddress
--  | opcode ==  "LDI" =   ldiop operands symbolsMap currentAddress
--  | opcode ==  "LDR" =   ldrop operands symbolsMap currentAddress
--  | opcode ==  "LEA" =   leaop operands symbolsMap currentAddress
--  | opcode ==  "NOT" =   notop operands symbolsMap currentAddress
--  | opcode ==  "RTI" =   "1000000000000000"
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
  | otherwise = error "INVALID OPCODE" 


addop :: [String] -> String
addop []  = ""
addop operands 
  -- If it's using the value from a register
  | Just resultRegister <- getRegister lastOperand = do
      firstPart ++ "000" ++ resultRegister
  | otherwise = do
      firstPart ++ "1" ++ imm5 lastOperand 
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
      firstPart ++ "1" ++ imm5 lastOperand 
  where lastOperand = last operands
        (dr:sr1:_) = operands 
        firstPart = "0101" ++ justGetRegister dr ++ justGetRegister sr1



-- 
-- brnop :: [String] -> M.Map String Integer -> Integer -> String
-- brzop :: [String] -> M.Map String Integer -> Integer -> String
-- brpop :: [String] -> M.Map String Integer -> Integer -> String
-- brop :: [String] -> M.Map String Integer -> Integer -> String
-- brzpop :: [String] -> M.Map String Integer -> Integer -> String
-- brnpop :: [String] -> M.Map String Integer -> Integer -> String
-- brnzop :: [String] -> M.Map String Integer -> Integer -> String
-- brnzpop :: [String] -> M.Map String Integer -> Integer -> String
-- jmpop :: [String] -> M.Map String Integer -> Integer -> String
-- retop :: [String] -> M.Map String Integer -> Integer -> String
-- jsrrop :: [String] -> M.Map String Integer -> Integer -> String
-- ldop :: [String] -> M.Map String Integer -> Integer -> String
-- ldiop :: [String] -> M.Map String Integer -> Integer -> String
-- ldrop :: [String] -> M.Map String Integer -> Integer -> String
-- leaop :: [String] -> M.Map String Integer -> Integer -> String
-- notop :: [String] -> M.Map String Integer -> Integer -> String
-- rtiop :: [String] -> M.Map String Integer -> Integer -> String
-- stop :: [String] -> M.Map String Integer -> Integer -> String
-- stiop :: [String] -> M.Map String Integer -> Integer -> String
-- strop :: [String] -> M.Map String Integer -> Integer -> String
-- trapop :: [String] -> M.Map String Integer -> Integer -> String
-- getcop :: [String] -> M.Map String Integer -> Integer -> String
-- outop :: [String] -> M.Map String Integer -> Integer -> String
-- putsop :: [String] -> M.Map String Integer -> Integer -> String
-- inop :: [String] -> M.Map String Integer -> Integer -> String
-- putspop :: [String] -> M.Map String Integer -> Integer -> String
-- haltop :: [String] -> M.Map String Integer -> Integer -> String

