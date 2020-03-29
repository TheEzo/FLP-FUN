-- Projekt: PLG-2-NKA
-- Login:   xwilla00
-- Autor:   Tomas Willaschek
-- Rok:     2020

import System.Environment
import Data.List.Split

import Types

main :: IO ()
main = do
  args <- getArgs
  f <- if ((length args) > 1) then readFile $ last args else getContents 
  let fileLines = lines f
  
  let nonTerminals = splitOn "," (head fileLines)
  
  let terminals = splitOn "," $ head $ drop 1 fileLines
  let start = head $ head $ drop 2 fileLines
  let rules = [(head splited, last splited) | x <- drop 3 fileLines, let splited = splitOn "->" x]

  -- validace gramatiky 
  --   jednoducha pravidla
  let g = Grammar nonTerminals terminals rules start
  print g

  case head args of "-i" -> print "-i"
                    "-1" -> print "-1"
                    "-2" -> print "-2"
                    _    -> error "Unknown argument..."

  ---------------

