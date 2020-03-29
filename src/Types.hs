module Types where
  data Grammar = Grammar { nonTerminals :: [String]           -- N
                         , terminals    :: [String]           -- T
                         , rules        :: [(String, String)] -- P
                         , start        :: Char               -- S
                         } deriving (Show)
