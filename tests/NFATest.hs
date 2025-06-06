-- file   = NFATest.hs
-- author = kansas

module Main where

import Test.Hspec
import qualified Data.Set as Set
import Auto.NFA


exampleNFA :: NFA
exampleNFA =
  let nfa0 = emptyNFA 0
      nfa1 = addTransition nfa0 0 Nothing 1 -- epsilon przejście: 0 -> 1
      nfa2 = addTransition nfa1 1 (Just 'a') 2 -- 1 --a--> 2
      nfa3 = addTransition nfa2 2 (Just 'b') 3 -- 2 --b--> 3
  in nfa3 { acceptStates = Set.singleton 3 }

main :: IO ()
main = hspec $ do
  describe "NFA acceptance" $ do
    it "accepts 'ab'" $
      acceptsNFA exampleNFA "ab" `shouldBe` True
    it "rejects 'a'" $
      acceptsNFA exampleNFA "a" `shouldBe` False
    it "rejects ''" $
      acceptsNFA exampleNFA "" `shouldBe` False
