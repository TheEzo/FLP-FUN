module Types where
  data Grammar = Grammar { nonTerminals :: [String]           -- N
                         , terminals    :: [String]           -- T
                         , rules        :: [(String, String)] -- P
                         , start        :: Char               -- S
                         } deriving (Show)

  data FSM = FSM { states      :: [Int]                       -- Q
                 -- , sigma       :: [String]                 -- sigma -- not needed
                 , delta       :: [(Int, (Char, Int))]      -- rules Pa->Q
                 , beginState  :: Int                         -- Start state
                 , finalStates :: [Int]                       -- Final states
                 } deriving (Show)
