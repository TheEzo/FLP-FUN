-- Projekt: PLG-2-NKA
-- Login:   xwilla00
-- Autor:   Tomas Willaschek
-- Rok:     2020

import System.Environment
import System.Exit
import Data.Char
import Data.List
import Data.List.Split
import Data.Function
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
  let rules = sortBy (on compare fst) [(head splited, last splited) |
                                       x <- drop 3 fileLines, let splited = splitOn "->" x]

  -- Validate non terminals and terminals in rules
  when (length [fst x | x <- rules, not $ elem (fst x) nonTerminals] > 0) $ raise "Rules contain unknown non-terminal"
  unless (elem [start] nonTerminals) $ raise "Start symbol must be non-terminal"
  -- when length [t | t <- terminals, elem t ['a'..'z']] > 0 $ raise "Terminal symbols contain prohibited symbol"
  -- unless (validateTerms terminals [snd x | x <- rules]) $ raise "Rules contain unknown terminal"

  -- end of validation

  let g = Grammar nonTerminals terminals rules start

  case head args of "-i" -> printGrammar g
                    "-1" -> transform g
                    "-2" -> print $ head args -- TODO
                    _    -> raise "Unknown argument..."


-- transform grammar to N->tN form
transform g = do
  -- group rules to determine same start non-terminal for different right sides
  let tmpGrp = [(x,z) | x <- nonTerminals g, let z = [y | (r,y) <- rules g, r == x]]
  -- generate list of new non-terminals (named after left side non-terminal) 
  -- for each right side
  let nonTermCnt = [z | x <- tmpGrp, let z = [if (length y) > 2
                                              then (length y) - 2
                                              else 0 | y <- snd x]]
  let tmpNonTerms = [reverse $ generateINonTerms (fst x) (snd x) |
                     x <- zip [sum x | x <- nonTermCnt] [fst x | x <- tmpGrp]]
  let newNonTerms = join [cutNonTerminals (fst x) (snd x) | x <- zip nonTermCnt tmpNonTerms]

  let newRules = concat [splitRule x | x <- zip newNonTerms $ rules g]

  let newG = Grammar ((nonTerminals g)++(join newNonTerms)) (terminals g) newRules (start g)
  printGrammar newG


  -- -- let neededNonTerminals = length [(x,z) | x <- nonTerminals g, let z = [y | (r,y) <- rules g, r == x]]
  -- let neededNonTerminals = length $ rules g

  -- -- vygenerovani novych neterminalu pro 3.2 gramatiku
  -- let tmpNonTerms = getNewNonTerminals neededNonTerminals $ nonTerminals g
  -- print tmpNonTerms
  -- putStrLn "---------------------------------"
  -- print $ rules g
  -- -- TODO: optimalizovat pocet
  -- let newNonTerms = [reverse $ tail $ generateINonTerms (fst x) (snd x) | 
  --                    x <- zip [if (length $ snd r) > 1 
  --                              then (length $ snd r) - 1 
  --                              else 1  | r <- rules g]
  --                             tmpNonTerms]
  -- print $ concat [splitRule r | r <- zip newNonTerms $ rules g]

--------------------------------------------------------------------------------------------------

------------------------------------------------
-- cut newly generated nonterminals to chunks --
------------------------------------------------
cutNonTerminals :: [Int] -> [String] -> [[String]]
cutNonTerminals [] _ = []
cutNonTerminals i n  = [getNHead (head i) n]++cutNonTerminals (tail i) (drop (head i) n)

-----------------------------------
-- get N times head from list xs --
-----------------------------------
getNHead :: Int -> [String] -> [String]
getNHead 0 xs = []
getNHead n xs = [head xs]++getNHead (n-1) (tail xs)

splitRule :: ([String],(String, String)) -> [(String, String)]
splitRule ([],(s,(x)))    = [(s,x)]
splitRule (h, (s,(x:xs))) = [(s,[x]++(head h))]++splitRule (tail h,(head h,xs))

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


generateINonTerms :: Int -> String -> [String]
generateINonTerms 0 x = []
generateINonTerms i x = list++(generateINonTerms j x)
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