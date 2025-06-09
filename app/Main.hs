-- file   = Main.hs
-- author = kansas

module Main where

import Auto.NFA
import Auto.DFA
import Core.NFAToDFA (nfaToDFA)
import Core.MinimizeDFA (minimizeDFA)
import Utils.GenerateRandomNFA (generateRandomNFA)
import Utils.ParseNFA (readNFAFromFile)

import qualified Data.Set as Set
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Auto.DFA as DFA
import qualified Data.Map as Map

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["random", statesStr, alphabetStr] -> runRandom statesStr alphabetStr
    ["file", filePath] -> runFromFile filePath
    ["debugMin"] -> runMinRedDFA
    _ -> printUsage

redundantDFA :: DFA.DFA
redundantDFA = DFA.DFA
  { DFA.states = Set.fromList [1, 2, 3, 4, 5]
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

runMinRedDFA :: IO()
runMinRedDFA = putStrLn $ prettyPrintDFA $ minimizeDFA redundantDFA

-- Tryb losowego NFA
runRandom :: String -> String -> IO ()
runRandom statesStr alphabetStr = do
  let maybeStates = readMaybeInt statesStr
  case maybeStates of
    Nothing -> putStrLn "Podaj liczbę stanów jako liczbę całkowitą." >> exitFailure
    Just n  -> do
      let alphabet = Set.fromList alphabetStr
      nfa <- generateRandomNFA n alphabet
      runWithNFA nfa

-- Tryb wczytania z pliku
runFromFile :: FilePath -> IO ()
runFromFile path = do
  nfa <- readNFAFromFile path
  runWithNFA nfa

-- Główne przetwarzanie NFA → DFA → min DFA
runWithNFA :: NFA -> IO ()
runWithNFA nfa = do
  putStrLn "=== NFA ==="
  putStrLn (prettyPrintNFA nfa)

  let dfa = nfaToDFA nfa
  putStrLn "\n=== DFA ==="
  putStrLn (prettyPrintDFA dfa)

  let minDfa = minimizeDFA dfa
  putStrLn "\n=== Min DFA ==="
  putStrLn (prettyPrintDFA minDfa)

-- Pomoc
printUsage :: IO ()
printUsage = do
  putStrLn "Użycie:"
  putStrLn "  cabal run project -- random <numStates> <alphabet>"
  putStrLn "  cabal run project -- file <pathToNFAFile>"
  putStrLn "Przykłady:"
  putStrLn "  cabal run project -- random 8 ab"
  putStrLn "  cabal run project -- file nfa.txt"
  exitFailure

readMaybeInt :: String -> Maybe Int
readMaybeInt s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing