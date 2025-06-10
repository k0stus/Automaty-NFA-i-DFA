-- file:   PrintNFA.hs
-- author: Tomasz Stefaniak

module Main where

import qualified Data.Set as Set
import Auto.NFA  -- Zakładając, że moduł Auto.NFA istnieje

complexNFA :: NFA
complexNFA = 
  let nfa = emptyNFA 1
      nfa1 = addTransition nfa 1 (Just 'a') 4
      nfa2 = addTransition nfa1 1 Nothing 2
      nfa3 = addTransition nfa2 2 (Just 'a') 3
      nfa4 = addTransition nfa3 3 Nothing 6
      nfa5 = addTransition nfa4 4 (Just 'a') 4
      nfa6 = addTransition nfa5 4 (Just 'b') 5
      nfa7 = addTransition nfa6 5 (Just 'a') 3
      nfa8 = addTransition nfa7 5 Nothing 1
  in  nfa8 { acceptStates = Set.singleton 6 }

main :: IO ()
main = do
    putStrLn "Pretty-printed NFA:"
    putStrLn $ prettyPrintNFA complexNFA
    putStrLn $ show $ epsilonClosure complexNFA (Set.singleton 1)
