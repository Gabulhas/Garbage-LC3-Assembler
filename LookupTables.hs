module LookupTables(
    opcodesMap,  
    registersMap,
    directivesMap,
    getDirective,
    getRegister,
    getOpcode,
    justGetRegister,
    justGetOpcode,
    justGetDirective
) where

import qualified Data.Map as M

-- Lookup tables

opcodes = [
    ("ADD", "0001"),
    ("AND", "0101"),
    ("BRN", "0000100"),
    ("BRZ", "0000010"),
    ("BRP", "0000001"),
    ("BR", "0000111"),
    ("BRZP", "0000011"),
    ("BRNP", "0000101"),
    ("BRNZ", "0000110"),
    ("BRNZP", "0000111"),
    ("JMP", "1100"),
    ("RET", "1000000111000000"),
    ("JSRR", "0100"),
    ("LD", "0010"),
    ("LDI", "1010"),
    ("LDR", "0110"),
    ("LEA", "1110"),
    ("NOT", "1001"),
    ("RTI", "1000000000000000"),
    ("ST", "0011"),
    ("STI", "1011"),
    ("STR", "0111"),
    ("TRAP", "1111"),
    ("GETC", "1111000000100000"),
    ("OUT", "1111000000100001"),
    ("PUTS", "1111000000100010"),
    ("IN", "1111000000100011"),
    ("PUTSP", "1111000000100100"),
    ("HALT", "1111000000100101")]

directives = [
    (".ORIG", ""),
    (".FILL", ""),
    (".BLKW", ""),
    (".STRINGZ", ""),
    (".END","")]


registers = [
    ("R0", "000"),
    ("R1", "001"),
    ("R2", "010"),
    ("R3", "011"),
    ("R4", "100"),
    ("R5", "101"),
    ("R6", "110"),
    ("R7", "111")
            ]

opcodesMap = M.fromList opcodes
directivesMap = M.fromList directives
registersMap = M.fromList registers


-- wrapperFunctions
getRegister :: String -> Maybe String 
getRegister key = M.lookup key registersMap

getOpcode :: String -> Maybe String 
getOpcode key = M.lookup key opcodesMap

getDirective :: String -> Maybe String 
getDirective key = M.lookup key directivesMap


justGetRegister :: String -> String
justGetRegister key 
  | Just val <- getRegister key = val
  | otherwise = error "Could not find Register"

justGetOpcode :: String -> String
justGetOpcode key 
  | Just val <- getOpcode key = val
  | otherwise = error "Could not find Opcode"

justGetDirective :: String -> String
justGetDirective key 
  | Just val <- getDirective key = val
  | otherwise = error "Could not find Directive"








