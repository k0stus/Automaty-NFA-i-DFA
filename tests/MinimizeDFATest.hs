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

redundantDFA2 :: DFA.DFA
redundantDFA2 = DFA.DFA
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
      , ((2, 'b'), 4)
      ] |> Map.fromList
  , DFA.startState = 1
  , DFA.acceptStates = Set.fromList [4, 5]
  }

(|>) :: a -> (a -> b) -> b
x |> f = f x

main :: IO ()
{-
main = hspec $ do
  describe "DFA minimization" $ do
    it "removes redundant states from a simple DFA" $ do
      let minimized = minimizeDFA redundantDFA
          minimizedStates = DFA.states minimized
      Set.size minimizedStates `shouldBe` 3

    it "retains correct accepting behavior" $ do
      let minimized = minimizeDFA redundantDFA
      DFA.acceptsDFA minimized "aa" `shouldBe` True
      DFA.acceptsDFA minimized "a"  `shouldBe` False
  -}    

main = do
  putStrLn $ prettyPrintDFA redundantDFA1
  putStrLn(prettyPrintDFA(minimizeDFA redundantDFA1))
  putStrLn $ prettyPrintDFA redundantDFA2
  putStrLn(prettyPrintDFA(minimizeDFA redundantDFA2))