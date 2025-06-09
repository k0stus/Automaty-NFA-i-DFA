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

moreComplexNFA :: NFA
moreComplexNFA = 
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
main = hspec $ do
  describe "NFA acceptance" $ do
    it "accepts 'ab'" $
      acceptsNFA exampleNFA "ab" `shouldBe` True
    it "rejects 'a'" $
      acceptsNFA exampleNFA "a" `shouldBe` False
    it "rejects ''" $
      acceptsNFA exampleNFA "" `shouldBe` False

  describe "NFA proper epsilon transition handling" $ do
    it "computes epsilon closure for state 0" $
      epsilonClosure exampleNFA (Set.singleton 0) `shouldBe` Set.fromList [0, 1]
