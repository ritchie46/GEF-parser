module Main where
import Data.List (intercalate)
import ParseGEF



main :: IO ()
main = do
  s <- readFile "app/a.gef"
  let gef = readGef s
  writeFile "test.csv" $ gefToCSVS gef
