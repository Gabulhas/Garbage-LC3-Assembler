import Data.Char (intToDigit)
import Data.Maybe (isJust)
import Numeric (showHex, showIntAtBase)
import Text.Printf
import Text.Regex.PCRE as R
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Binary as B
import Data.Word as W


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


operands = [
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

operandsMap = M.fromList operands
directivesMap = M.fromList directives
registersMap = M.fromList registers

whiteTextRegex = "[^\\s\"\']+|\"([^\"]*)\"|\'([^\']*)\'\r\n"


readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile 


cleanAndSplit :: String -> [String]
cleanAndSplit cLine = R.getAllTextMatches (cLine =~ whiteTextRegex)


removeCommentsAndEmpty :: [String] -> [String]
removeCommentsAndEmpty [] = []
removeCommentsAndEmpty (x:xs) 
    | x == "" = removeCommentsAndEmpty xs
    | head x == ';' = []
    | otherwise  = x:removeCommentsAndEmpty xs


printLines :: [[String]] -> IO ()
printLines [] = putStrLn ""
printLines (x:xs) =  do 
                     print x
                     printLines xs


clean :: [String] -> [[String]] -> [[String]]
clean [] accum = accum
clean (x:xs) accum
    | null result = clean xs accum
    | otherwise = clean xs (result : accum)
    where result = removeCommentsAndEmpty (cleanAndSplit x)

prepareForRead :: [String] -> [[String]]
prepareForRead [] = [[]]
prepareForRead text_lines = 
    reverse (clean text_lines [])


getFirstAddress :: [[String]] -> Integer 
getFirstAddress [] = -1
getFirstAddress (x:_) = 
    read  (tail (x !! 1)) :: Integer 


firstPass :: [[String]] -> M.Map String Integer -> Integer -> M.Map String Integer 
firstPass [] resultSymbolicMap _ = resultSymbolicMap
firstPass (x:xs) resultSymbolicMap currentAddress 
  | foundResult = firstPass xs resultSymbolicMap (currentAddress + 1)
  | otherwise  = do 
                firstPass xs (M.insert currentHead (currentAddress + 1) resultSymbolicMap) (currentAddress + 1)
  where currentHead = head x
        foundResult = isJust (M.lookup currentHead operandsMap) || isJust (M.lookup currentHead directivesMap)



tokenToBinary :: String -> M.Map String Integer -> Integer -> String
tokenToBinary [] _ _ = ""
tokenToBinary token symbolsMap currentAddress
  | Just x <- M.lookup token symbolsMap = 
  | Just x <- M.lookup token directivesMap = x
  | Just x <- M.lookup token operandsMap = x
  | Just x <- M.lookup token registersMap = x
      -- TODO: CHANGE!!
  | otherwise = ""


assembleLine :: [String] -> M.Map String Integer -> Integer -> String
-- this edge case never happens tho
assembleLine [] _ _ = ""
assembleLine (x:xs) symbolsMap currentAddress
  | isJust (M.lookup x symbolsMap) = assembleLine xs symbolsMap currentAddress 
  | otherwise = concatMap (\ x -> tokenToBinary x symbolsMap currentAddress) (x:xs)
    
-- Oneliners btw
secondPass :: [[String]] -> M.Map String Integer -> Integer -> [String] -> String
secondPass [] _ _ resultString = concat (reverse resultString)
secondPass (x:xs) symbolsMap currentAddress resultString = do 
                let resultLine = assembleLine x symbolsMap currentAddress
                secondPass xs symbolsMap (currentAddress + 1) (resultLine : resultString)

main :: IO ()
main = readLines "sample/2048.asm" >>= \s -> 
    do
        let result = prepareForRead s
        let firstAddress = getFirstAddress result
        let resultingMap = firstPass result M.empty firstAddress
        let resultBinary = secondPass result resultingMap firstAddress []
        print resultBinary


