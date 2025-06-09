-- file   = DFA.hs
-- author = Tomasz Stefaniak

module Auto.DFA
  ( DFA(..)
  , Transition
  , emptyDFA
  , addTransition
  , acceptsDFA
  , prettyPrintDFA
  ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)

import Auto.NFA (State, Symbol) -- Importujemy State i Symbol z NFA, aby uniknąć duplikacji kodu

-- DFA transitions: z jednego stanu i konretnego symbolu przechodzimy do dokładnie jednego stanu
type Transition = Map (State, Symbol) State

data DFA = DFA
  { states       :: Set State
  , alphabet     :: Set Symbol
  , transition   :: Transition
  , startState   :: State
  , acceptStates :: Set State
  } deriving (Show, Eq)

-- Tworzy pusty DFA z jednym stanem startowym
emptyDFA :: State -> DFA
emptyDFA start = DFA
  { states = Set.singleton start
  , alphabet = Set.empty
  , transition = Map.empty
  , startState = start
  , acceptStates = Set.empty
  }

-- Dodaje przejście w DFA z jednego stanu do drugiego przez konkretny symbol
addTransition :: DFA -> State -> Symbol -> State -> DFA
addTransition dfa from sym to =
  let key       = (from, sym)
      oldTrans  = transition dfa
      newTrans  = Map.insert key to oldTrans
      newStates = Set.insert from $ Set.insert to (states dfa)
      newAlphabet = Set.insert sym (alphabet dfa)
  in dfa { transition = newTrans, states = newStates, alphabet = newAlphabet }

--W DFA nie ma epsilon wobec tego nie ma potrzeby definiowania epsilonClosure

-- Sprawdza, czy DFA akceptuje dane słowo
acceptsDFA :: DFA -> String -> Bool
acceptsDFA dfa input = go (startState dfa) input
  where
    go :: State -> String -> Bool
    go currState [] = currState `Set.member` acceptStates dfa
    go currState (x:xs) =
      case Map.lookup (currState, x) (transition dfa) of
        Just nextState -> go nextState xs
        Nothing        -> False  -- Jeśli brak przejścia to odrzucamy

-- Funkcja do ładnego wypisania struktury DFA
prettyPrintDFA :: DFA -> String
prettyPrintDFA dfa =
  let stateList = Set.toList $ states dfa
      alphaList = Set.toList $ alphabet dfa
      transList = Map.toList $ transition dfa
      transLines =
        [ show q ++ " -" ++ [a] ++ "-> " ++ show r
        | ((q, a), r) <- transList ]
  in unlines $
       [ "States: " ++ show stateList
       , "Alphabet: " ++ show alphaList
       , "Start state: " ++ show (startState dfa)
       , "Accepting states: " ++ show (Set.toList $ acceptStates dfa)
       , "Transitions:" ] ++ map ("  " ++) transLines
