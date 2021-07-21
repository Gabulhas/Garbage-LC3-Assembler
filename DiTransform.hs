module DiTransform (
    diTransform,
    getDiSize
) where
import AssemblerUtils (dirValueToBin, hexToInt, bitExtension, toStringBinary)
import Data.Char (ord, readLitChar)

diTransform :: [String] -> String
diTransform (di:argument) 
  | di == ".END" = ""
  | di == ".FILL" = dirValueToBin (head argument)
  | di == ".BLKW" = blkw (head argument)
  | di == ".STRINGZ" = stringz (head argument)
  | otherwise = error ("Invalid directive " ++ di)

diTransform di = error ("Invalid directive " ++ unwords di)

blkw :: String -> String
blkw argument = 
    concat $ replicate (calculateBLKWsize argument) "0000000000000000"

stringz :: String -> String
stringz argument=
    stringzAux (removeQuotes argument) [] ++ "0000000000000000"


stringzAux :: String -> [String] -> String
stringzAux "" result = concat $ reverse result
stringzAux currentString result = do
    let (currentChar, nextString) = head $ readLitChar currentString
    let currentResult = bitExtension  (toStringBinary (ord currentChar)) 16 '0'
    stringzAux nextString (currentResult : result)



stringUnescape :: String -> String
stringUnescape a = stringUnescapeAux a ""

stringUnescapeAux :: String -> String -> String
stringUnescapeAux "" result = reverse result
stringUnescapeAux currentString result = do
    let (currentChar, nextString) = head $ readLitChar currentString
    stringUnescapeAux nextString (currentChar : result)



removeQuotes :: String -> String
removeQuotes "" = ""
removeQuotes (x:xs) = 
    init xs


getDiSize :: [String] -> Int
getDiSize [] = 0
getDiSize (di:argument) 
  | di == ".BLKW" =  2 * calculateBLKWsize (head argument) 
  | di == ".STRINGZ"  =  length (stringUnescape $head argument) - 1
  | otherwise = 0


calculateBLKWsize :: String -> Int
calculateBLKWsize argument 
    | head argument == 'x' = hexToInt $ tail argument
    | head argument == '#' = read $ tail argument
    | otherwise = error "Invalid literal value, use # or X"

