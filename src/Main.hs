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
  unless (elem [start] nonTerminals) $ raise "Start symbol must be non-terminal"
  -- when length [t | t <- terminals, elem t ['a'..'z']] > 0 $ raise "Terminal symbols contain prohibited symbol"
  -- unless (validateTerms terminals [snd x | x <- rules]) $ raise "Rules contain unknown terminal"

  -- end of validation

  let g = Grammar nonTerminals terminals rules start

  case head args of "-i" -> printGrammar g
                    "-1" -> transform g
                    "-2" -> print $ head args
                    _    -> raise "Unknown argument..."

  -- putStrLn "---------------------------------"
  -- -- ["A", "B", "C"] --> ["A1", "B2", "C3"]
  -- print $ nonT 0 nonTerminals
  -- putStrLn "---------------------------------"
  
  -- -- rules to [(N, [N right sides])] -- bude potreba pro KA asi
  -- print [(x,z) | x <- nonTerminals, let z = [y | (r,y) <- rules, r == x]]
  -- putStrLn "---------------------------------"
  -- -- kaslu na setreni znaky zatim
  -- -- let neededNonTerminals = length  [(x,z) | x <- nonTerminals, let z = [y | (r,y) <- rules, r == x], length z > 1]
  -- let neededNonTerminals = length [(x,z) | x <- nonTerminals, let z = [y | (r,y) <- rules, r == x]]
  -- print $ splitR "A" "aaB"


  -- putStrLn "---------------------------------"
  -- -- vygenerovani novych neterminalu pro 3.2 gramatiku
  -- print nonTerminals
  -- let newNonTerminals = getNewNonTerminals neededNonTerminals nonTerminals


-- transform grammar to N->tN form
transform g = do
  -- let neededNonTerminals = length [(x,z) | x <- nonTerminals g, let z = [y | (r,y) <- rules g, r == x]]
  let neededNonTerminals = length $ rules g

  -- vygenerovani novych neterminalu pro 3.2 gramatiku
  let newNonTerminals = getNewNonTerminals neededNonTerminals $ nonTerminals g
  print newNonTerminals
  putStrLn "---------------------------------"
  print $ rules g

  --TODO: generate NT1..NTn
  print [splitRule r | r <- zip newNonTerminals (rules g)]
  print $ nonT 5 "H"
-- start symbol -> right side -> [(start, right side)] :: A -> xB
-- splitRule :: String -> String -> [(String, String)]
-- splitRule :: String -> (String, String) -> String
splitRule (h,(s,(x:xs))) = if length xs > 1
                           then [(h,h)] -- [(s,x++(head h))]++splitRule (tail h,(head $ tail h,xs))
                           else [(h,xs)] -- (s++"->"++[x]++xs)    h++s++[x]++xs 


-- generate i non terminals that are not already present in xs
getNewNonTerminals :: Int -> [String] -> [String]
getNewNonTerminals 0 xs = []
getNewNonTerminals i xs = nonternimal++getNewNonTerminals (i-1) (xs++nonternimal)
  where
  	nonternimal = [generateNonTerminal [] xs]

-- generate NT which is not present in xs
generateNonTerminal :: String -> [String] -> String
generateNonTerminal [] xs   = generateNonTerminal "A" xs
generateNonTerminal prev xs = if elem prev xs
                              then generateNonTerminal [succ $ head prev] xs
                              else prev


nonT :: Int -> String -> [String]
nonT 0 _ = []
nonT i x = list++(nonT j x)
  where
    j = (getLastI list) - 1
    list = [x ++ (show i) :: String]

getLastI :: [String] -> Int
getLastI [] = 0
getLastI x  = read(tail $ last x) :: Int


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

-- validateTerms :: [String] -> [String] -> Bool
-- validateTerms xs (y:ys) = if elem y xs || head y == '#'
--                           then validateTerms xs ys 
--                           else (if isUpper $ head y then validateTerms xs ys else False) 
-- validateTerms xs []     = True