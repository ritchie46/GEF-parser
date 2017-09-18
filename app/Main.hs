module Main where
import System.Directory
import System.Environment
import ParseGEF
import Data.List (isInfixOf)
import Data.Char (toLower)
import Control.Monad (filterM)
import Data.Text (replace, pack, unpack)


-- "C:/Users/vik/Desktop/empty/test/"
main :: IO ()
main = do
    args <- getArgs

    if length args == 0
        then do putStrLn "gef-parser - Convert .GEF to .CSV files.\n\n"
                putStrLn "Usage: gef-parser.exe [filepath.gef] [filepath.csv]\n \
                          \                     [directory (all .gef files in that directory are converted)]"
        else do
            let fstArg = args !! 0
            isdir <- doesDirectoryExist fstArg
            if isdir
                then do
                    putStrLn $ "Converting the .gef files in :" ++ fstArg
                    if last fstArg `elem` "\\/"
                        then convertDir fstArg
                        else convertDir $ fstArg ++ "/"
                else convertFile fstArg $ args !! 1


convertFile :: String -> String -> IO ()
convertFile fn_in fn_out = do
    contents <- readFile fn_in
    let csvContents = gefToCSVS $ readGef contents
    putStrLn $ "Converting " ++ fn_in ++ " to " ++ fn_out ++ "..."
    writeFile fn_out csvContents


convertDir :: String -> IO ()
convertDir path = do

    listdir_ <- getDirectoryContents path
    -- filter gef files                     $ [Filenames] to Lowercase
    let listdir = filter (isInfixOf ".gef") $ map (map toLower) listdir_

    -- replace .gef with .csv and append to path.
    let outNames = map ((path ++) . unpack . replace (pack ".gef") (pack ".csv") . pack) listdir

    -- read the contents of all gef files.
    contents <- mapM (readFile . (path ++)) $ take (length listdir - 2) listdir
    let gef = map readGef contents

    let csvContents = map gefToCSVS gef

    a <- sequence $ zipWith writeFile outNames csvContents
    putStrLn "Convered the following files:\n"
    print outNames
