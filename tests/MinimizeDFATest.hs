-- file   = minTest.hs
-- author = kansas

module Main where

import Test.Hspec
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Auto.DFA as DFA
import Core.MinimizeDFA (minimizeDFA)
import Auto.DFA (prettyPrintDFA)

redundantDFA1 :: DFA.DFA
redundantDFA1 = DFA.DFA
  { DFA.states = Set.fromList [0, 1, 2, 3, 4, 5]
  , DFA.alphabet = Set.fromList ['a', 'b']
  , DFA.transition = 
      [ ((0, 'a'), 1)
      , ((0, 'b'), 2)
      , ((1, 'a'), 0)
      , ((1, 'b'), 3)
      , ((2, 'a'), 4)
      , ((2, 'b'), 5)
      , ((3, 'a'), 4)
      , ((3, 'b'), 5)
      , ((4, 'a'), 4)
      , ((4, 'b'), 5)
      , ((5, 'a'), 5)
      , ((5, 'b'), 5)
      ] |> Map.fromList
  , DFA.startState = 1
  , DFA.acceptStates = Set.fromList [2, 3, 4]
  }

redundantDFA2 = DFA.DFA
  { DFA.states = Set.fromList [0,1,2,3,4]
  , DFA.alphabet = Set.fromList ['x']
  , DFA.transition = Map.fromList
      [ ((0, 'x'), 1)
      , ((1, 'x'), 2)
      , ((2, 'x'), 2)
      -- stany 3 i 4 są martwe
      , ((3, 'x'), 4)
      , ((4, 'x'), 3)
      ]
  , DFA.startState = 0
  , DFA.acceptStates = Set.fromList [2]
  }

redundantDFA3 = DFA.DFA
  { DFA.states = Set.fromList [0, 1, 2, 3]
  , DFA.alphabet = Set.fromList ['a']
  , DFA.transition = Map.fromList
      [ ((0, 'a'), 1)
      , ((1, 'a'), 2)
      , ((2, 'a'), 3)
      , ((3, 'a'), 3)
      ]
  , DFA.startState = 0
  , DFA.acceptStates = Set.fromList [2, 3]
  }
-- Stany 2 i 3 są nierozróżnialne — zawsze prowadzą do stanu akceptującego.

redundantDFA4 :: DFA.DFA
redundantDFA4 = DFA.DFA
  { DFA.states = Set.fromList [1, 2, 3, 4, 5, 6, 7]
  , DFA.alphabet = Set.fromList ['a', 'b']
  , DFA.transition = 
      [ ((1, 'a'), 2)
      , ((1, 'b'), 3)
      , ((2, 'b'), 4)
      , ((3, 'b'), 5)
      , ((3, 'a'), 4)
      , ((4, 'b'), 5)
      , ((5, 'a'), 2)
      , ((5, 'b'), 4)
      ] |> Map.fromList
  , DFA.startState = 1
  , DFA.acceptStates = Set.fromList [4, 5]
  }

(|>) :: a -> (a -> b) -> b
x |> f = f x

main :: IO ()

main = do
  putStrLn "\n=== DFA # 1 ==="
  putStrLn $ prettyPrintDFA redundantDFA1
  putStrLn(prettyPrintDFA(minimizeDFA redundantDFA1))
  putStrLn     "==============="
  putStrLn "\n\n=== DFA # 2 ==="
  putStrLn $ prettyPrintDFA redundantDFA2
  putStrLn(prettyPrintDFA(minimizeDFA redundantDFA2))
  putStrLn     "==============="
  putStrLn "\n\n=== DFA # 3 ==="
  putStrLn $ prettyPrintDFA redundantDFA3
  putStrLn(prettyPrintDFA(minimizeDFA redundantDFA3))
  putStrLn     "==============="
  putStrLn "\n\n=== DFA # 4 ==="
  putStrLn $ prettyPrintDFA redundantDFA4
  putStrLn(prettyPrintDFA(minimizeDFA redundantDFA4))
