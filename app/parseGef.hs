import System.IO
import System.Environment
import Control.Monad
import Data.Typeable
import Text.Regex.Posix
import Data.List.Split
import Data.Maybe
import Text.Regex.PCRE.Heavy

-- Note that the regex type is POSIX this differs from Python and Perl

strToFlt :: [Char] -> Float
strToFlt = read

strListToFltList :: [String] -> [Float]
strListToFltList = map read

splitComma :: Maybe String -> [String]
splitComma x = splitOn "," (fromJust x)

sliceRange :: Integer -> Integer -> [a] -> [a]
sliceRange a b (x:xs)
    | a == 0 = [x] ++ sliceRange (a - 1) (b - 1) xs
    | b == 0 = [x]
    | a < 0 && b > 0 = [x] ++ sliceRange (a - 1) (b - 1) xs
    | otherwise = sliceRange (a - 1) (b - 1) xs

-- assignTuple :: [a] -> (a)
-- assignTuple x:xs = (x, assignTuple xs)

main = do
    (fn:x) <- getArgs
    putStrLn ("Parsing " ++ fn ++ "...")

    handle <- openFile fn ReadMode
    contents <- hGetContents handle

    let gef_lines = lines contents
    let z_match = contents =~~ "#ZID.+" :: Maybe String
    let z = strToFlt $ splitComma z_match!!1

    let xy_match = contents =~~ "#XYID.+" :: Maybe String
    let [x, y] = sliceRange 1 2 (strListToFltList $ splitComma xy_match)

    print(contents =~~ "(?<=#COLUMN\\D.)\\d+|(?<=#COLUMN\\D..)\\d+|(?<=#COLUMN\\D)\\d+" :: Maybe String)
    print(x, y)
