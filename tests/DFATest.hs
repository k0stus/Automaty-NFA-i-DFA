-- file   = DFATest.hs
-- author = Tomasz Stefaniak

module Main where

import Test.Hspec
import qualified Data.Set as Set
import Auto.DFA


-- Przykładowy DFA: akceptuje słowa kończące się na "ab"
exampleDFA :: DFA
exampleDFA =
  let dfa0 = emptyDFA 0
      dfa1 = addTransition dfa0 0 'a' 1 -- 0 --a--> 1
      dfa2 = addTransition dfa1 1 'b' 2 -- 1 --b--> 2
  in dfa2 { acceptStates = Set.singleton 2 }

main :: IO ()
main = hspec $ do
  describe "acceptsDFA" $ do
    it "akceptuje 'ab'" $
      acceptsDFA exampleDFA "ab" `shouldBe` True

    it "nie akceptuje 'a'" $
      acceptsDFA exampleDFA "a" `shouldBe` False

    it "nie akceptuje 'b'" $
      acceptsDFA exampleDFA "b" `shouldBe` False

    it "nie akceptuje ''" $
      acceptsDFA exampleDFA "" `shouldBe` False

    it "nie akceptuje 'abc'" $
      acceptsDFA exampleDFA "abc" `shouldBe` False
