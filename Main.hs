-- file   = Main.hs
-- author = kansas

module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import NFA

epsilonClosure :: NFA -> Set State -> Set State
epsilonClosure nfa states0 = go states0 Set.empty
  where
    go todo visited
      | Set.null todo = visited
      | otherwise =
          let (s, rest) = Set.deleteFindMin todo
              visited' = Set.insert s visited
              epsilons = Map.findWithDefault Set.empty (s, Nothing) (transition nfa)
              newStates = Set.difference epsilons visited'
          in go (Set.union rest newStates) visited'

-- Sprawdza, czy NFA akceptuje dane słowo (rekurencyjnie, BFS)
acceptsNFA :: NFA -> String -> Bool
acceptsNFA nfa input = go (epsilonClosure nfa (Set.singleton $ startState nfa)) input
  where
    go :: Set State -> String -> Bool
    go currStates [] =
      not $ Set.null $ Set.intersection currStates (acceptStates nfa)
    go currStates (x:xs) =
      let stepTargets = Set.unions
            [ Map.findWithDefault Set.empty (state, Just x) (transition nfa)
            | state <- Set.toList currStates ]
          closure = epsilonClosure nfa stepTargets
      in go closure xs

-- Przykładowy automat: akceptuje słowa kończące się na 'ab'
exampleNFA :: NFA
exampleNFA =
  let nfa0 = emptyNFA 0
      nfa1 = addTransition nfa0 0 Nothing 1          -- epsilon przejście: 0 -> 1
      nfa2 = addTransition nfa1 1 (Just 'a') 2       -- 1 --a--> 2
      nfa3 = addTransition nfa2 2 (Just 'b') 3       -- 2 --b--> 3
  in nfa3 { acceptStates = Set.singleton 3 }

-- Testujemy automat
main :: IO ()
main = do
  putStrLn "=== NFA Structure ==="
  putStrLn $ prettyPrintNFA exampleNFA

  let tests = ["ab", "a", "b", "", "abc"]
  mapM_ (\w -> putStrLn $ w ++ " -> " ++ show (acceptsNFA exampleNFA w)) tests