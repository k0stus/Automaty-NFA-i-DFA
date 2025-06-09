-- file   = minTest.hs
-- author = kansas

module Main where

import Test.Hspec
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Auto.DFA as DFA
import Core.MinimizeDFA (minimizeDFA)

-- Przykładowy DFA z redundancją: stany 1 i 2 są identyczne
redundantDFA :: DFA.DFA
redundantDFA = DFA.DFA
  { DFA.states = Set.fromList [0, 1, 2, 3]
  , DFA.alphabet = Set.fromList ['a']
  , DFA.transition = 
      -- Stan 0 przechodzi do 1 lub 2 przez 'a'
      -- Stany 1 i 2 przechodzą do 3, który jest stanem akceptującym
      -- czyli: 0 -a-> 1, 1 -a-> 3
      --        0 -a-> 2, 2 -a-> 3
      -- Te przejścia tworzą redundantne ścieżki
      [ ((0, 'a'), 1)
      , ((1, 'a'), 3)
      , ((2, 'a'), 3)
      ] |> Map.fromList
  , DFA.startState = 0
  , DFA.acceptStates = Set.fromList [3]
  }

(|>) :: a -> (a -> b) -> b
x |> f = f x

main :: IO ()
main = hspec $ do
  describe "DFA minimization" $ do
    it "removes redundant states from a simple DFA" $ do
      let minimized = minimizeDFA redundantDFA
          minimizedStates = DFA.states minimized
          -- Spodziewamy się 3 stanów: 0, {1,2}, 3
      Set.size minimizedStates `shouldBe` 3

    it "retains correct accepting behavior" $ do
      let minimized = minimizeDFA redundantDFA
      DFA.acceptsDFA minimized "aa" `shouldBe` True
      DFA.acceptsDFA minimized "a"  `shouldBe` False