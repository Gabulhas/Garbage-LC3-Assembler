import Data.Char (intToDigit)
import Data.List
import Data.Maybe (isJust)
import Numeric (showHex, showIntAtBase)
import Text.Printf
import Text.Regex.PCRE as R
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Binary as B
import Data.Word as W
import OpTransform (opTransform)
import DiTransform (diTransform, getDiSize)
import LookupTables (getOpcode, getRegister, getDirective)
import AssemblerUtils (dirValueToBin, hexToInt)


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
    reverse $ clean text_lines []


getOrigin :: [[String]] -> String
getOrigin ((dir:origin:_):_) 
  | dir == ".ORIG" = origin
  | otherwise = error "Invalid Origin Directive"
getOrigin _ = error "Invalid Origin Directive"


-- I don't like this :(
handleLabel :: [String] -> M.Map String Int -> Int -> (M.Map String Int, Int)
handleLabel [] _ _ = error "Invalid Label"
handleLabel [label] resultSymbolicMap currentAddress = (M.insert label currentAddress resultSymbolicMap, currentAddress + 1)

handleLabel (label:possibleCode) resultSymbolicMap currentAddress
  | isJust (getOpcode argHead) = (newMap, currentAddress + 1)
  | isJust (getDirective argHead) = (newMap, currentAddress + getDiSize possibleCode)
  | otherwise = (newMap, currentAddress + 1)
  where newMap = M.insert label currentAddress resultSymbolicMap
        argHead = head possibleCode

firstPass :: [[String]] -> M.Map String Int -> Int -> M.Map String Int 
firstPass [] resultSymbolicMap _ = resultSymbolicMap
firstPass ([]:xs) _ _ = error "Invalid Opcode"
firstPass (("":a):xs) _ _ = error "Invalid Opcode"
firstPass (x:xs) resultSymbolicMap currentAddress 
  | isJust (getDirective currentHead) = firstPass xs resultSymbolicMap (currentAddress + getDiSize x)
  | foundResult = firstPass xs resultSymbolicMap (currentAddress + 1)
  | otherwise  = do 
      let (newMap, newAddress) = handleLabel x resultSymbolicMap currentAddress
      firstPass xs newMap newAddress
  where currentHead = head x
        
        foundResult = isJust (getOpcode currentHead)


handleLine :: [String] -> M.Map String Int -> Int -> String
-- this edge case never happens tho
handleLine [] _ _ = ""
handleLine (x:xs) symbolsMap currentAddress
  | head x == '.' = diTransform (x:xs)
  | isJust (M.lookup x symbolsMap) = handleLine xs symbolsMap currentAddress 
  | otherwise = opTransform (x:xs) symbolsMap currentAddress
    

secondPass :: [[String]] -> M.Map String Int -> Int -> [String] -> String
secondPass [] _ _ resultString = concat (reverse resultString)
secondPass (x:xs) symbolsMap currentAddress resultString = do 
                let resultLine = handleLine x symbolsMap currentAddress
                -- TODO: remove, is temporary
                if mod (length resultLine) 16 == 0 then secondPass xs symbolsMap (currentAddress + 1) (resultLine : resultString)
                                                   else error ("\nINVALID NUMBER OF CHARS AT" ++ unwords x)
                -- only .END returns ""
                                    

main :: IO ()
main = readLines "sample/rogue.asm" >>= \s -> 
    do
        let result = prepareForRead s
        let origin = getOrigin result
        print result
        -- change read to hexToInt 
        let firstAddress = hexToInt $ tail origin
        let initialCode = [dirValueToBin origin]
        let resultingMap = firstPass (tail result) M.empty firstAddress
        let resultBinary = secondPass (tail result) resultingMap firstAddress initialCode
        print resultBinary


