module Main where
import System.Directory
import System.Environment
import ParseGEF
import Data.List (isInfixOf)
import Data.Char (toLower)
import Control.Monad (filterM)


-- "C:/Users/vik/Desktop/empty/test/"
main :: IO ()
main = do
    args <- getArgs

    if length args == 0
        then do putStrLn "gef-parser - Convert .GEF to .CSV.\n\n"
                putStrLn "Usage: gef-parser.exe [filename.gef] [filename.csv]\n \
                          \                     [directory (all .gef files in that directory are converted)]"
        else do
            let fstArg = args !! 0
            isdir <- doesDirectoryExist fstArg
            if isdir
                then convertDir fstArg
                else do
                    print ""



convertDir :: String -> IO ()
convertDir path = do

    listdir_ <- getDirectoryContents path
    let listdir = filter (isInfixOf ".gef" . map toLower) listdir_
    -- let listdir = filter (isInfixOf ".gef") listdir_
    contents <- mapM (readFile . (path ++)) $ take (length listdir - 2) listdir
    let gef = map readGef contents

    let csvContents = map gefToCSVS gef
    let names = writeGefPaths path 0 csvContents

    a <- sequence $ zipWith writeFile names csvContents
    print names

writeGefPaths :: String -> Int -> [String] -> [String]
writeGefPaths _ _ [] = [""]
writeGefPaths p c (x:xs) = (p ++ show c ++ ".csv") : writeGefPaths p (c + 1) xs
