-- file   = NFA.hs
-- author = kansas

module Auto.NFA
  ( State
  , Symbol
  , NFA(..)
  , Transition
  , emptyNFA
  , addTransition
  , epsilonClosure
  , acceptsNFA
  , prettyPrintNFA
  ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import Data.Maybe (fromMaybe)

type State = Int
type Symbol = Char
-- NFA transitions: z jednego stanu i opcjonalnego symbolu przechodzimy do zbioru stanów
type Transition = Map (State, Maybe Symbol) (Set State)

data NFA = NFA
  { states       :: Set State
  , alphabet     :: Set Symbol
  , transition   :: Transition
  , startState   :: State
  , acceptStates :: Set State
  } deriving (Show, Eq)

-- Tworzy pusty NFA z jednym stanem startowym
emptyNFA :: State -> NFA
emptyNFA start = NFA
  { states = Set.singleton start
  , alphabet = Set.empty
  , transition = Map.empty
  , startState = start
  , acceptStates = Set.empty
  }

-- Dodaje przejście w NFA z jednego stanu do zbioru stanów przez opcjonalny symbol
addTransition :: NFA -> State -> Maybe Symbol -> State -> NFA
addTransition nfa from maybeSym to =
  let key        = (from, maybeSym)
      oldTrans   = transition nfa
      updatedSet = Set.insert to $ Map.findWithDefault Set.empty key oldTrans
      newTrans   = Map.insert key updatedSet oldTrans
      newStates  = Set.insert from $ Set.insert to (states nfa) -- Ensure both states are in the NFA
      newAlphabet = case maybeSym of
        Just sym -> Set.insert sym (alphabet nfa) -- Add symbol to alphabet if it's not Nothing
        Nothing  -> alphabet nfa
  in nfa { transition = newTrans, states = newStates, alphabet = newAlphabet }

-- Funkcja obliczająca domknięcie epsilonowe dla zbioru stanów (epsilon reprezentowany przez Nothing)
epsilonClosure :: NFA -> Set State -> Set State
epsilonClosure nfa states0 = go states0 Set.empty
  where
    go todo visited
      | Set.null todo = visited
      | otherwise =
          let (s, rest) = Set.deleteFindMin todo
              visited' = Set.insert s visited
              epsilons = Map.findWithDefault Set.empty (s, Nothing) (transition nfa)
              newStates = Set.difference epsilons visited'
          in go (Set.union rest newStates) visited'

-- Sprawdza, czy NFA akceptuje dane słowo (rekurencyjnie, BFS)
acceptsNFA :: NFA -> String -> Bool
acceptsNFA nfa input = go (epsilonClosure nfa (Set.singleton $ startState nfa)) input
  where
    go :: Set State -> String -> Bool
    go currStates [] =
      not $ Set.null $ Set.intersection currStates (acceptStates nfa)
    go currStates (x:xs) =
      let stepTargets = Set.unions
            [ Map.findWithDefault Set.empty (state, Just x) (transition nfa)
            | state <- Set.toList currStates ]
          closure = epsilonClosure nfa stepTargets
      in go closure xs

-- Funkcja do ładnego wypisania struktury NFA
prettyPrintNFA :: NFA -> String
prettyPrintNFA nfa =
  let stateList = Set.toList $ states nfa
      alphaList = Set.toList $ alphabet nfa
      transList = Map.toList $ transition nfa
      showSymbol (Just c) = [c]
      showSymbol Nothing  = "ε"
      transLines =
        [ show q ++ " -" ++ showSymbol a ++ "-> " ++ show (Set.toList qs)
        | ((q, a), qs) <- transList ]
  in unlines $
       [ "States: " ++ show stateList
       , "Alphabet: " ++ show alphaList
       , "Start state: " ++ show (startState nfa)
       , "Accepting states: " ++ show (Set.toList $ acceptStates nfa)
       , "Transitions:" ] ++ map ("  " ++) transLines
  