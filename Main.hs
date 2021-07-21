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
import DiTransform (diTransform)
import LookupTables (getOpcode, getRegister, getDirective)
import AssemblerUtils (dirValueToBin)


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


getOrigin :: [[String]] -> String
getOrigin ((dir:origin:_):_) 
  | dir == ".ORIG" = origin
  | otherwise = error "Invalid Origin Directive"
getOrigin _ = error "Invalid Origin Directive"


firstPass :: [[String]] -> M.Map String Int -> Int -> M.Map String Int 
firstPass [] resultSymbolicMap _ = resultSymbolicMap
firstPass (x:xs) resultSymbolicMap currentAddress 
  | foundResult = firstPass xs resultSymbolicMap (currentAddress + 1)
  | otherwise  = do 
                firstPass xs (M.insert currentHead currentAddress resultSymbolicMap) (currentAddress + 1)
  where currentHead = head x
        foundResult = isJust (getOpcode currentHead) || isJust (getDirective currentHead)


handleLine :: [String] -> M.Map String Int -> Int -> String
-- this edge case never happens tho
handleLine [] _ _ = ""
handleLine (x:xs) symbolsMap currentAddress
  | head x == '.' = diTransform (x:xs)
  | isJust (M.lookup x symbolsMap) = handleLine xs symbolsMap currentAddress 
  | otherwise = opTransform (x:xs) symbolsMap currentAddress
    
-- Oneliners btw
secondPass :: [[String]] -> M.Map String Int -> Int -> [String] -> String
secondPass [] _ _ resultString = concat (reverse resultString)
secondPass (x:xs) symbolsMap currentAddress resultString = do 
                let resultLine = handleLine x symbolsMap currentAddress
                secondPass xs symbolsMap (currentAddress + 1) (resultLine : resultString)
                -- only .END returns ""
                                    

main :: IO ()
main = readLines "sample/2048.asm" >>= \s -> 
    do
        let result = prepareForRead s
        let origin = getOrigin result
        let firstAddress = read (tail origin)
        let initialCode = [dirValueToBin origin]
        let resultingMap = firstPass (tail result) M.empty firstAddress
        let resultBinary = secondPass (tail result) resultingMap firstAddress initialCode
        print resultBinary


