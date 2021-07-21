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
import OpTransform (assembleLine)
import LookupTables (getOpcode, getRegister, getDirective)


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


getFirstAddress :: [[String]] -> Int 
getFirstAddress [] = -1
getFirstAddress (x:_) = 
    read  (tail (x !! 1)) :: Int 


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
  | isJust (M.lookup x symbolsMap) = handleLine xs symbolsMap currentAddress 
  | otherwise = assembleLine (x:xs) symbolsMap currentAddress
    
-- Oneliners btw
secondPass :: [[String]] -> M.Map String Int -> Int -> [String] -> String
secondPass [] _ _ resultString = unwords (reverse resultString)
secondPass (x:xs) symbolsMap currentAddress resultString = do 
                let resultLine = handleLine x symbolsMap currentAddress
                secondPass xs symbolsMap (currentAddress + 1) (resultLine : resultString)

main :: IO ()
main = readLines "sample/basic.asm" >>= \s -> 
    do
        let result = prepareForRead s
        let firstAddress = getFirstAddress result
        print firstAddress
        let resultingMap = firstPass (tail result) M.empty firstAddress
        print resultingMap
        let resultBinary = secondPass (tail result) resultingMap firstAddress []
        print resultBinary


