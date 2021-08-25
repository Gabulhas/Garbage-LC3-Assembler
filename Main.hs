import AssemblerUtils (dirValueToBin, hexToInt)
import Data.Binary as B
import Data.Char (intToDigit)
import Data.List
import Data.ByteString as BS  (ByteString, writeFile)
import qualified Data.ByteString.Char8 as BT
import Data.Maybe (isJust) 
import Data.Word as W
import DiTransform (diTransform, getDiSize)
import LookupTables (getOpcode, getRegister, getDirective)
import Numeric (showHex, showIntAtBase)
import OpTransform (opTransform)
import System.Environment (getArgs)
import Data.Text.Encoding (encodeUtf8)
import Text.Printf
import Text.Regex.PCRE as R
import qualified Data.Map as M
import qualified Data.Text as T



whiteTextRegex = "[^\\s\"\']+|\"([^\"]*)\"|\'([^\']*)\'\r\n"


readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile 


splitOnSpace :: String -> [String]
splitOnSpace cLine = R.getAllTextMatches (cLine =~ whiteTextRegex)


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
    where result = removeCommentsAndEmpty (splitOnSpace x)

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
handleLabel :: [String] -> M.Map String Int -> Int -> Int
handleLabel [] _ _ = error "Invalid Label"
handleLabel [label] resultSymbolicMap currentAddress = currentAddress

handleLabel (label:possibleCode) resultSymbolicMap currentAddress
  | isJust (getOpcode argHead) = currentAddress + 1
  | isJust (getDirective argHead) = currentAddress + getDiSize possibleCode
  | otherwise = currentAddress
  where 
        argHead = head possibleCode

-- TOFIX
firstPass :: [[String]] -> M.Map String Int -> Int -> M.Map String Int 
firstPass [] resultSymbolicMap _ = resultSymbolicMap
firstPass ([]:xs) _ _ = error "Invalid Opcode"
firstPass (("":a):xs) _ _ = error "Invalid Opcode"
firstPass (x:xs) resultSymbolicMap currentAddress 
  | isJust (getDirective currentHead) = firstPass xs resultSymbolicMap (currentAddress + getDiSize x)
  | isJust (getOpcode currentHead) = firstPass xs resultSymbolicMap (currentAddress + 1)
  | otherwise  = do 
      let newAddress = handleLabel x resultSymbolicMap currentAddress
      let newMap = M.insert (head x) currentAddress resultSymbolicMap
      firstPass xs newMap newAddress
  where currentHead = head x

handleLine :: [String] -> M.Map String Int -> Int -> String
-- this edge case never happens tho
handleLine [] _ _ = ""
handleLine (x:xs) symbolsMap currentAddress
  | head x == '.' = diTransform (x:xs) symbolsMap
  | isJust (M.lookup x symbolsMap) = handleLine xs symbolsMap currentAddress 
  | otherwise = opTransform (x:xs) symbolsMap currentAddress
    

secondPass :: [[String]] -> M.Map String Int -> Int -> [String] -> String
secondPass [] _ _ resultString = concat (reverse resultString)
secondPass (x:xs) symbolsMap currentAddress resultString
    | isJust (M.lookup (head x) symbolsMap) && null (tail x)  = secondPass xs symbolsMap currentAddress resultString
    | otherwise = do
                let resultLine = handleLine x symbolsMap currentAddress
                secondPass xs symbolsMap (currentAddress + 1) (resultLine : resultString)
-- TOFIX


assemblePipeLine :: [String] -> String
assemblePipeLine textLines = do
        let result = prepareForRead textLines
        let origin = getOrigin result
        -- change read to hexToInt 
        let firstAddress = hexToInt $ tail origin
        let initialCode = [dirValueToBin origin]
        let resultingMap = firstPass (tail result) M.empty firstAddress
        secondPass (tail result) resultingMap firstAddress initialCode


packStr'' :: String -> BS.ByteString
packStr'' = encodeUtf8 . T.pack

assembleFile :: String -> String -> IO ()
assembleFile fileName outfile= readLines fileName >>= \s -> do
        BS.writeFile outfile (packStr'' (assemblePipeLine s))


handleArgs :: [String] -> IO ()
handleArgs [] = printf  "%s\n" "Error. Usage: assembler INFILE [OUTFILE] or assembler INFILE"

handleArgs (infile:xs) = do
    let outfile = if null xs then "OUT.BIN" else head xs
    assembleFile infile outfile



main :: IO ()
main = do
    args <- getArgs
    handleArgs args
