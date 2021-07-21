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
    ("ADD", ""),
    ("AND", ""),
    ("BR", ""),
    ("BRN", ""),
    ("BRNP", ""),
    ("BRNZ", ""),
    ("BRNZP", ""),
    ("BRP", ""),
    ("BRZ", ""),
    ("BRZP", ""),
    ("JMP", ""),
    ("RET", ""),
    ("JSR", ""),
    ("JSRR", ""),
    ("LD", ""),
    ("LDI", ""),
    ("LDR", ""),
    ("LEA", ""),
    ("NOT", ""),
    ("RTI", ""),
    ("ST", ""),
    ("STI", ""),
    ("STR", ""),
    ("TRAP", ""),
    ("GETC", ""),
    ("OUT", ""),
    ("PUTS", ""),
    ("IN", ""),
    ("PUTSP", ""),
    ("HALT", "")]

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
  | otherwise = error ("Could not find Register '" ++ key ++ "'")

justGetOpcode :: String -> String
justGetOpcode key 
  | Just val <- getOpcode key = val
  | otherwise = error "Could not find Opcode"

justGetDirective :: String -> String
justGetDirective key 
  | Just val <- getDirective key = val
  | otherwise = error "Could not find Directive"
