module DiTransform (
    diTransform,
    getDiSize
) where
import AssemblerUtils (dirValueToBin, hexToInt, bitExtension, toStringBinary)
import Data.Char (ord, readLitChar)

diTransform :: [String] -> String
diTransform (di:arguments) 
  | di == ".END" = ""
  | di == ".FILL" = filld arguments
  | di == ".BLKW" = blkw arguments
  | di == ".STRINGZ" = stringz arguments
  | otherwise = error ("Invalid directive " ++ di)

diTransform di = error ("Invalid directive " ++ unwords di)


filld :: [String] -> String
filld [] = error "FILL requires at least one argument."
filld (argument:_) = 
    dirValueToBin argument


blkw :: [String] -> String
blkw [] = error "BLKW requires at least one argument."
blkw (argument:_) = 
    concat $ replicate (calculateBLKWsize [argument]) "0000000000000000"

stringz :: [String] -> String
stringz [] = error "STRINGZ requires at least one argument."
stringz (argument:_)=
    stringzAux (removeQuotes argument) [] ++ "0000000000000000"


-- remove?
stringzAux :: String -> [String] -> String
stringzAux "" result = concat $ reverse result
stringzAux currentString result
  | null currentPart = concat $ reverse result
  | otherwise = do
    let (currentChar, nextString) = head $ readLitChar currentString
    let currentResult = bitExtension  (toStringBinary (ord currentChar)) 16 '0'
    stringzAux nextString (currentResult : result)
    where currentPart = readLitChar currentString



stringUnescape :: String -> String
stringUnescape a = stringUnescapeAux a ""

stringUnescapeAux :: String -> String -> String
stringUnescapeAux "" result = reverse result
stringUnescapeAux currentString result
    | null currentPart = result
    | otherwise = do
        let (currentChar, nextString) = head $ readLitChar currentString
        stringUnescapeAux nextString (currentChar : result)
    where currentPart = readLitChar currentString




removeQuotes :: String -> String
removeQuotes "" = ""
removeQuotes (x:xs) = 
    init xs


getDiSize :: [String] -> Int
getDiSize [] = 0
getDiSize [di] = 0
getDiSize (di:argument) 
  | di == ".BLKW" =  2 * calculateBLKWsize argument
  | di == ".STRINGZ"  =  calculateSTRINGZsize argument
  | otherwise = 0


calculateBLKWsize :: [String] -> Int
calculateBLKWsize [] = error "BLKW requires at least one argument."
calculateBLKWsize (argument:_) 
    | head argument == 'x' = hexToInt $ tail argument
    | head argument == '#' = read $ tail argument
    | otherwise = error "Invalid literal value, use # or X"

calculateSTRINGZsize :: [String] -> Int
calculateSTRINGZsize [] = error "STRINGZ requires at least one argument."
calculateSTRINGZsize (argument:_) = 
    length (stringUnescape argument) - 1


