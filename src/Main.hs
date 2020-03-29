-- Projekt: PLG-2-NKA
-- Login:   xwilla00
-- Autor:   Tomas Willaschek
-- Rok:     2020

import System.Environment
import System.Exit
import Data.Char
import Data.List
import Data.List.Split
import Control.Monad

import Types

main :: IO ()
main = do
  args <- getArgs
  case length args of 0 -> raise "Program requires aruments"
                      1 -> if validateArgs args then putStr "" else raise $ "Invalid argument "++head args
                      2 -> if validateArgs args then putStr "" else raise $ "Invalid argument "++head args
                      _ -> raise "Only two arguments at the same time are supported"

  f <- if length args > 1 then readFile $ last args else getContents 
  let fileLines = lines f
  
  let nonTerminals = splitOn "," $ head fileLines
  let terminals = splitOn "," $ head $ drop 1 fileLines
  let start = head $ head $ drop 2 fileLines
  let rules = [(head splited, last splited) | x <- drop 3 fileLines, let splited = splitOn "->" x]

  -- Validate non terminals and terminals in rules
  when (length [fst x | x <- rules, not $ elem (fst x) nonTerminals] > 0) $ raise "Rules contain unknown non-terminal"
  unless (validateTerms terminals [snd x | x <- rules]) $ raise "Rules contain unknown terminal"

  -- end of validation

  let g = Grammar nonTerminals terminals rules start

  case head args of "-i" -> printGrammar g
                    "-1" -> print $ head args
                    "-2" -> print $ head args
                    _    -> raise "Unknown argument..."

raise :: String -> IO a
raise str = die str

validateArgs :: [String] -> Bool
validateArgs []    = False
validateArgs (x:_) = x == "-i" || x == "-1" || x == "-2"

printGrammar :: Grammar -> IO ()
printGrammar g = do
  putStrLn $ intercalate "," $ nonTerminals g
  putStrLn $ intercalate "," $ terminals g
  putStrLn [start g]
  putStrLn $ intercalate "\n" [fst x++"->"++snd x | x <- rules g]

validateTerms :: [String] -> [String] -> Bool
validateTerms xs (y:ys) = if elem y xs 
                          then validateTerms xs ys 
                          else (if isUpper $ head y then validateTerms xs ys else False) 
validateTerms xs []     = True