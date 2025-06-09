-- file   = DFATest.hs
-- author = Tomasz Stefaniak

module Main where

import Test.Hspec
import qualified Data.Set as Set

import Auto.NFA (prettyPrintNFA)
import Utils.GenerateRandomNFA

main :: IO ()
main = hspec $ do
    describe "generateRandomNFA" $ do
        it "generates a valid NFA " $ do
            nfa <- generateRandomNFA 15 (Set.fromList ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'])
            putStrLn (prettyPrintNFA nfa)
