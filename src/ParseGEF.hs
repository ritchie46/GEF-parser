module ParseGEF
    (
    readGef,
    gefToCSVS
    ) where

import Text.ParserCombinators.ReadP
import Data.Char
import Data.List (isInfixOf, intercalate)

type GefColumns = [(Integer, String, String)]
type GefValues = [[Float]]
type Gef = (GefColumns, GefValues)

-- Helper functions for parsing patterns

isVowel :: Char -> Bool
isVowel char =
    char `elem` "aouei"

vowel :: ReadP Char
vowel =
    satisfy isVowel

isAlphabetic :: Char -> Bool
isAlphabetic char =
    char `elem` ['A'..'Z'] || char `elem` ['a'..'z']

alphabetic :: ReadP Char
alphabetic = satisfy isAlphabetic

nonAlphabetic :: ReadP Char
nonAlphabetic = satisfy (not . isAlphabetic)

digit :: ReadP Char
digit = satisfy  isDigit

upperCase :: ReadP Char
upperCase = satisfy (\char -> 'A' <= char && char <= 'Z')

atLeastOneVowel :: ReadP String
atLeastOneVowel = many1 vowel

isNummeric :: Char -> Bool
isNummeric char =
  char `elem` ['0'..'9'] || char `elem` ".e-+,"

isNotNummeric :: Char -> Bool
isNotNummeric = not . isNummeric

columnInfo :: ReadP (Integer, String, String)
-- test with:  readP_to_S  columnInfo "#COLUMNINFO= 1, m, sondeerlengte, 1"
columnInfo = do
    skipMany nonAlphabetic
    string "COLUMNINFO"
    skipMany (satisfy  (`elem` "= "))
    column <- digit
    skipMany nonAlphabetic
    unit <- munch isAlphabetic
    skipMany nonAlphabetic
    name <- munch isAlphabetic
    -- other option
    -- name <- many alphabetic

    -- return :: a -> m a
    -- return is a function that return a Monad of type a
    -- In this case turns String in ReadP String
    return (read [column], unit, name)

-- Determine the column info.
-- Param 1: All the lines from a .GEF file
-- Out: [(column_id, unit, measurement)]
detColumnInfo :: [String] -> [(Integer, String, String)]
detColumnInfo [] = []
detColumnInfo (x:xs)
            | null result = detColumnInfo xs
            | otherwise    = fst  (last result): detColumnInfo xs
            where
              result = readP_to_S columnInfo x

-- Take the lines of a .GEF file and return the line index where the #EOH is located
endOfHeader :: [String] -> Int
endOfHeader = flip _endOfHeader 0

_endOfHeader :: [String] -> Int -> Int
_endOfHeader (x:xs) n
  | "EOH" `isInfixOf` x = n
  | otherwise = _endOfHeader xs n + 1

-- Helper function: Parse the nummeric values of the dataframe in a GEF file.
csvVal :: ReadP String
csvVal = do
        skipMany . satisfy $ isNotNummeric
        many . satisfy $ isNummeric

-- detCsvValLine "0.0000e+000 9.9990e+003 9.9990e+003 9.9990e+003"
-- Parse all the nummeric values of a DataFrame line in a GEF file
detCsvValLine :: String -> [Maybe Float]
detCsvValLine [] = []
--                   prepend Float         to    []         (string)
detCsvValLine ln =  _reads (fst result): detCsvValLine (snd result)
  where
    -- ReadP_to_S returns (
    -- ("val", "remaining string")
    -- ("val2", "remaining string")
    -- ("val3", "remaining string")
    -- )
    -- result = ("last-val", "remaining string")
    result = last $ readP_to_S csvVal ln

_reads :: String -> Maybe Float
_reads [] = Nothing
_reads s = Just (fst (last (reads s)))

_removeNothing :: [Maybe a] -> [a]
_removeNothing [] = []
_removeNothing (x:xs) = case x of Nothing -> _removeNothing xs
                                  Just x -> x: _removeNothing xs

readGef :: String -> Gef
readGef s = let ln = lines s
            in let ci = detColumnInfo ln
                   eoh = endOfHeader ln
                   -- _removeNothing because of the possible parsing errors.
                   in let valLines = map (_removeNothing . detCsvValLine) $ drop (eoh + 1) ln
                          in (ci, valLines)

gefToCSVS :: Gef -> String
gefToCSVS (ci, valLines) = let header = intercalate "," (map (map repl . show) ci)
                               values = map (intercalate "," . map show) valLines
                               in unlines $ header : values
                               where
                                 repl ',' = ':'
                                 repl c = c


-- gefToCSVS (ci, valLines) = unlines $ concatMap show ci : map (intercalate "," . map show) valLines
