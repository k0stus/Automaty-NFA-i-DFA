-- file   = NFA.hs
-- author = kansas

module NFA
  ( State
  , Symbol
  , NFA(..)
  , Transition
  , emptyNFA
  , addTransition
  , prettyPrintNFA
  ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import Data.Maybe (fromMaybe)

type State = Int
type Symbol = Char

type Transition = Map (State, Maybe Symbol) (Set State)

data NFA = NFA{ 
    states       :: Set State
  , alphabet     :: Set Symbol
  , transition   :: Transition
  , startState   :: State
  , acceptStates :: Set State
  } deriving (Show, Eq)

emptyNFA :: State -> NFA
emptyNFA start = NFA{ 
    states = Set.singleton start
  , alphabet = Set.empty
  , transition = Map.empty
  , startState = start
  , acceptStates = Set.empty
  }

addTransition :: NFA -> State -> Maybe Symbol -> State -> NFA
addTransition nfa from mSym to =
  let oldTrans = transition nfa
      key = (from, mSym)
      updatedSet = Set.insert to (Map.findWithDefault Set.empty key oldTrans)
      newTrans = Map.insert key updatedSet oldTrans
      newStates = Set.insert from $ Set.insert to (states nfa)
      newAlphabet = case mSym of
        Just sym -> Set.insert sym (alphabet nfa)
        Nothing  -> alphabet nfa       
  in nfa { transition = newTrans, states = newStates, alphabet = newAlphabet }

prettyPrintNFA :: NFA -> String
prettyPrintNFA nfa =
  let stateList = Set.toList (states nfa)
      alphaList = Set.toList (alphabet nfa)
      transList = Map.toList (transition nfa)
      showSymbol :: Maybe Symbol -> String
      showSymbol (Just c) = [c]
      showSymbol Nothing  = "ε"
      transLines = [ show q ++ " -" ++ showSymbol a ++ "-> " ++ show (Set.toList qs)
                   | ((q, a), qs) <- transList ]
  in unlines $
       [ "States: " ++ show stateList
       , "Alphabet: " ++ show alphaList
       , "Start state: " ++ show (startState nfa)
       , "Accepting states: " ++ show (Set.toList (acceptStates nfa))
       , "Transitions:" ] ++ map ("  " ++) transLines