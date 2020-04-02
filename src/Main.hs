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

---------------------------------------------------------------
-- read input, parse it to grammar and execute argument code --
---------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case length args of 0 -> raise "Program requires aruments"
                      1 -> unless (validateArgs args) $ raise $ "Invalid argument "++head args
                      2 -> unless (validateArgs args) $ raise $ "Invalid argument "++head args
                      _ -> raise "Only two arguments at the same time are supported"

  f <- if length args > 1 then readFile $ last args else getContents 
  let fileLines = lines f
  
  let nonTerminals = splitOn "," $ head fileLines
  let terminals = splitOn "," $ fileLines !! 1
  let start = head $ fileLines !! 2
  let rules = sortBy (on compare fst) [(head splited, last splited) |
                                       x <- drop 3 fileLines, let splited = splitOn "->" x]

  -- Validate non terminals and terminals in rules
  unless (null [x | x <- terminals, head x `notElem` ['a'..'z']]) $ 
       raise "Terminals must be from [a..z]"
  unless (null [x | x <- nonTerminals, head x `notElem` ['A'..'Z']]) $ 
       raise "Non-terminals must be from [A..Z]"
  unless (null [fst x | x <- rules, fst x `notElem`  nonTerminals]) $ 
       raise "Rules contain unknown non-terminal on left side"
  unless (null [x | x <- nonTerminals, x `notElem` [fst x | x <- rules]]) $
       raise "Non-terminals contain unused symbol"
  unless ([start] `elem` nonTerminals) $ raise "Start symbol must be non-terminal"
  unless (validateRightSide (terminals,nonTerminals) rules) $ 
       raise "Right side of rule must be in format A->xB, where x belongs to terminals"
  -- end of validation

  let g = Grammar nonTerminals terminals rules start

  case head args of "-i" -> printGrammar g
                    "-1" -> transformG g False
                    "-2" -> transformG g True
                    _    -> raise "Unknown argument..."

-----------------------------------------------------------
-- check if right side of rule is xB where x is terminal --
-----------------------------------------------------------
validateRightSide :: ([String],[String]) -> [(String, String)] -> Bool
validateRightSide _ []                               = True
validateRightSide (terms,nonTerms) rules | t && nT   = validateRightSide (terms,nonTerms) 
                                                                         (tail rules)
                                         | otherwise = False
  where rule = snd $ head rules
        t    = checkTerms terms $ init rule
        nT   = checkTerms nonTerms [last rule]

------------------------------------
-- check if x is present in terms --
------------------------------------
checkTerms :: [String] -> String -> Bool
checkTerms _ []                            = True
checkTerms terms (x:xs) | [x] `elem` terms = checkTerms terms xs
                        | x == '#'         = checkTerms terms xs
                        | otherwise        = False

---------------------------------------------------------------
-- transform grammar to A->aB form and then to FSM if needed --
---------------------------------------------------------------
transformG :: Grammar -> Bool -> IO ()
transformG g fsm = do
  -- group rules to determine same start non-terminal for different right sides
  let tmpGrp = [(x,z) | x <- nonTerminals g, let z = [y | (r,y) <- rules g, r == x]]
  -- generate list of new non-terminals (named after left side non-terminal) 
  -- for each right side
  let nonTermCnt = [z | x <- tmpGrp, let z = [if length y > 2
                                              then length y - 2
                                              else 0 | y <- snd x]]
  let tmpNonTerms = [reverse $ uncurry generateINonTerms x |
                     x <- zip [sum x | x <- nonTermCnt] [fst x | x <- tmpGrp]]
  let newNonTerms = join [uncurry cutNonTerminals x | x <- zip nonTermCnt tmpNonTerms]
  -- split rules to A->aB form with use of new non-terminals
  let newRules = concat [splitRule x | x <- zip newNonTerms $ rules g]
  let newG = Grammar (nonTerminals g ++ join newNonTerms) (terminals g) newRules (start g)
  
  if fsm
  then printFSM $ transformFSM newG
  else printGrammar newG

------------------------------
-- transform grammar to FSM --
------------------------------
transformFSM :: Grammar -> FSM
transformFSM g   = FSM states delta begin finals
  where states   = removeDuplicates $ [fst x | x <- delta]++[snd $ snd x | x <- delta]
        delta    = sortBy (on compare fst) newRules
        begin    = snd $ head [x | x <- map, fst x == [start g]]
        finals   = getFinalStates map (rules g)
        map      = getDictionary (rules g) []
        newRules = convertRules map [r | r <- rules g, snd r /= "#"]

----------------------------------------------
-- convert grammar rules to FSM delta rules --
----------------------------------------------
convertRules :: [(String, Int)] -> [(String, String)] -> [(Int, (Char, Int))]
convertRules _ []          = []
convertRules map rules = (lNumber, (term, rNumber)) : convertRules map (tail rules)
  where rule    = head rules
        lNumber = snd $ head [x | x <- map, fst x == fst rule]
        term    = head $ snd rule
        rNumber = snd $ head [x | x <- map, fst x == tail (snd rule)]

--------------------------
-- get FSM final states --
--------------------------
getFinalStates :: [(String, Int)] -> [(String, String)] -> [Int]
getFinalStates _       []                      = []
getFinalStates map rules | snd rule == "#" = s++getFinalStates map (tail rules)
                             | otherwise       = getFinalStates map (tail rules)
  where s    = [snd $ head [x | x <- map, fst x == fst rule]]
        rule = head rules

-------------------------------------------------------------------
-- get dictionary for converting grammar to fsm => (NonTerm,Num) --
-------------------------------------------------------------------
getDictionary :: [(String, String)] -> [(String, Int)] -> [(String, Int)]
getDictionary [] fsm = fsm
getDictionary g fsm  = if fst (head g) `elem` [fst x | x <- fsm]
                       then getDictionary (tail g) fsm
                       else getDictionary (tail g) (fsm++[(fst $ head g,i)])
  where list = if null fsm then [] else ["a"++show (snd x) :: String | x <- fsm]
        i    = getLastI list + 1

------------------------------------------------
-- cut newly generated nonterminals to chunks --
------------------------------------------------
cutNonTerminals :: [Int] -> [String] -> [[String]]
cutNonTerminals [] _ = []
cutNonTerminals i n  = getNHead (head i) n : cutNonTerminals (tail i) (drop (head i) n)

-----------------------------------
-- get N times head from list xs --
-----------------------------------
getNHead :: Int -> [String] -> [String]
getNHead 0 xs = []
getNHead n xs = head xs : getNHead (n-1) (tail xs)

------------------------------------------
-- remove duplicated elements from list --
------------------------------------------
removeDuplicates :: [Int] -> [Int]
removeDuplicates []                   = []
removeDuplicates (x:xs) | x `elem` xs = removeDuplicates xs
                        | otherwise   = x : removeDuplicates xs

------------------------------------------------
-- split rule (A->aaB) to [(A->aA1),(A1->aB)] --
------------------------------------------------
splitRule :: ([String],(String, String)) -> [(String, String)]
splitRule ([],(s,x))      = [(s,x)]
splitRule (h, (s,x:xs)) = (s,x : head h) : splitRule (tail h,(head h,xs))

-----------------------------------------------------------------
-- generate i non terminals that are not already present in xs --
-----------------------------------------------------------------
getNewNonTerminals :: Int -> [String] -> [String]
getNewNonTerminals 0 xs = []
getNewNonTerminals i xs = nonternimal++getNewNonTerminals (i-1) (xs++nonternimal)
  where
    nonternimal = [generateNonTerminal [] xs]

--------------------------------------------
-- generate NT which is not present in xs --
--------------------------------------------
generateNonTerminal :: String -> [String] -> String
generateNonTerminal [] xs   = generateNonTerminal "A" xs
generateNonTerminal prev xs = if prev `elem` xs
                              then generateNonTerminal [succ $ head prev] xs
                              else prev

------------------------------------------------------------
-- generate i numbered duplicates of x => 2 A -> [A2, A1] --
------------------------------------------------------------
generateINonTerms :: Int -> String -> [String]
generateINonTerms 0 x = []
generateINonTerms i x = list++generateINonTerms j x
  where
    j = getLastI list - 1
    list = [x ++ show i :: String]

---------------------------------------------------
-- get number of last element => [A1, A22] -> 22 --
---------------------------------------------------
getLastI :: [String] -> Int
getLastI [] = 0
getLastI x  = read(tail $ last x) :: Int

------------------------------------------
-- raise error message and exit program --
------------------------------------------
raise :: String -> IO a
raise str = error str

--------------------------------
-- validate program arguments --
--------------------------------
validateArgs :: [String] -> Bool
validateArgs []    = False
validateArgs (x:_) = x == "-i" || x == "-1" || x == "-2"

-------------------------------------
-- print grammar in correct format --
-------------------------------------
printGrammar :: Grammar -> IO ()
printGrammar g = do
  putStrLn $ intercalate "," $ sort $ nonTerminals g
  putStrLn $ intercalate "," $ sort $ terminals g
  putStrLn [start g]
  putStrLn $ intercalate "\n" [fst x++"->"++snd x | x <- rules g]

---------------------------------
-- print FSM in correct format --
---------------------------------
printFSM :: FSM -> IO ()
printFSM fsm = do
  putStrLn $ intercalate "," [show x :: String | x <- states fsm]
  putStrLn (show $ beginState fsm :: String)
  putStrLn $ intercalate "," [show x :: String | x <- finalStates fsm]
  putStrLn $ intercalate "\n" [(show $ fst x :: String)++","++
                               [fst $ snd x]++","++
                               (show $ snd $ snd x :: String) |
                               x <- delta fsm]
