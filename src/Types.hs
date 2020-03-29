module Types where
  data Grammar = Grammar { nonTerminals :: [String]           -- N
                         , terminals    :: [String]           -- T
                         , rules        :: [(String, String)] -- P
                         , start        :: Char               -- S
                         } deriving (Show)

  data FSM = FSM { states      :: [Int]
                 , sigma       :: [String]
                 , delta       :: [(Int, (String, Int))]
                 , beginState  :: Int
                 , finalStates :: [Int] 
                 } deriving (Show)
