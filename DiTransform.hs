module DiTransform (
    diTransform
) where
import AssemblerUtils (dirValueToBin, hexToInt, bitExtension, toStringBinary)
import Data.Char (ord)

diTransform :: [String] -> String
diTransform (di:argument) 
  | di == ".END" = ""
  | di == ".FILL" = dirValueToBin (head argument)
  | di == ".BLKW" = blkw (head argument)
  | di == ".STRINGZ" = stringz (head argument)
  | otherwise = error ("Invalid directive " ++ di)

diTransform di = error ("Invalid directive " ++ unwords di)

blkw :: String -> String
blkw argument
    | head argument == 'x' = concat $ replicate (hexToInt $ tail argument) "0000000000000000"
    | head argument == '#' = concat $ replicate (read $ tail argument) "0000000000000000"
    | otherwise = error "Invalid literal value, use # or X"

stringz :: String -> String
stringz argument=
    
    concatMap (\a -> bitExtension  (toStringBinary (ord a)) 16 '0') (removeQuotes argument) ++ "0000000000000000"


removeQuotes :: String -> String
removeQuotes "" = ""
removeQuotes (x:xs) = 
    init xs

