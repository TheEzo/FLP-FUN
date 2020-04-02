-- Projekt: PLG-2-NKA
-- Login:   xwilla00
-- Autor:   Tomas Willaschek
-- Rok:     2020

module Types where
  ------------------------------------------
  -- type represents right linear grammar --
  ------------------------------------------
  data Grammar = Grammar { nonTerminals :: [String]           -- N
                         , terminals    :: [String]           -- T
                         , rules        :: [(String, String)] -- P
                         , start        :: Char               -- S
                         } deriving (Show)

  ---------------------------------------------------------------------
  -- type represents Finite State Machine with all needed attributes --
  ---------------------------------------------------------------------
  data FSM = FSM { states      :: [Int]                       -- Q
                 -- , sigma       :: [String]                 -- sigma -- not needed
                 , delta       :: [(Int, (Char, Int))]        -- rules Pa->Q
                 , beginState  :: Int                         -- Start state
                 , finalStates :: [Int]                       -- Final states
                 } deriving (Show)
