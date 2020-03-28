-- Projekt: PLG-2-NKA
-- Login:   xwilla00
-- Autor:   Tomas Willaschek
-- Rok:     2020

import System.Environment
import Data.List.Split

main :: IO ()
main = do
    args <- getArgs
    let file = last args
    case head args of "-i" -> print "-i"
                      "-1" -> print "-1"
                      "-2" -> print "-2"
                      _    -> error "Unknown argument..."
    
    f <- readFile file
    let l = lines f
    print (splitOn "," (head l))

