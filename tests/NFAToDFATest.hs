-- file   = NFAToDFATest.hs
-- author = Tomasz Stefaniak

module Main where

import Test.Hspec
import qualified Data.Set as Set
import System.IO (hFlush, stdout)

import Auto.NFA (prettyPrintNFA)
import Auto.DFA (prettyPrintDFA)
import Utils.GenerateRandomNFA
import Core.NFAToDFA (nfaToDFA)

main :: IO ()
main = hspec $ do
    describe "nfaToDFA" $ do
        it "convert NFA to DFA" $ do
            nfa <- generateRandomNFA 15 (Set.fromList ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'])
            putStrLn (prettyPrintNFA nfa)

            let dfa = nfaToDFA nfa
            putStrLn (prettyPrintDFA dfa)
            hFlush stdout -- Wymuszenie wypisania na konsolę przed zakończeniem testu
