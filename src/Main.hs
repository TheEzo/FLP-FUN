-- Projekt: PLG-2-NKA
-- Login:   xwilla00
-- Autor:   Tomas Willaschek
-- Rok:     2020

import System.Environment

main :: IO ()
main = do
    x <- getArgs
    print x
    -- args ["-h"] = print "ahoj"
    