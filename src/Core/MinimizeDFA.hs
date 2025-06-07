-- file   = MinimizeDFA
-- author = kansas

module Core.MinimizeDFA (minimizeDFA) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import Data.List (find)
import Data.Maybe (fromJust)

import Auto.DFA

-- Funkcja minimalizacji deterministycznego automatu skończonego
minimizeDFA :: DFA -> DFA
minimizeDFA dfa =
  let
    -- Inicjalny podział: stany akceptujące i nieakceptujące
    initialPartition = [acceptStates dfa, Set.difference (states dfa) (acceptStates dfa)]

    -- Iteracyjne dzielenie grup, aż do ustabilizowania
    refine :: [Set State] -> [Set State]
    refine groups =
      let groups' = refineOnce groups
      in if groups' == groups then groups else refine groups'

    refineOnce :: [Set State] -> [Set State]
    refineOnce groups = concatMap (splitGroup groups) groups

    -- Dzieli grupę na podgrupy na podstawie przejść
    splitGroup :: [Set State] -> Set State -> [Set State]
    splitGroup groups group =
      let classify state = [ findGroup (move state a) groups | a <- Set.toList (alphabet dfa) ]
          buckets = Map.fromListWith Set.union
                      [ (classify s, Set.singleton s) | s <- Set.toList group ]
      in Map.elems buckets

    -- Zwraca grupę, w której znajduje się stan
    findGroup :: State -> [Set State] -> Set State
    findGroup s = fromJust . find (Set.member s)

    -- Funkcja przejścia
    move :: State -> Symbol -> State
    move s a = Map.findWithDefault (-1) (s, a) (transition dfa)

    -- Minimalne stany = numerujemy grupy od 0
    groups = refine initialPartition
    stateMap :: Map (Set State) State
    stateMap = Map.fromList $ zip groups [0..]

    -- Budujemy nowe przejścia
    newTransitions =
      [ ((stateMap Map.! group, a), stateMap Map.! findGroup (move (Set.findMin group) a) groups)
      | group <- groups
      , a <- Set.toList (alphabet dfa)
      , move (Set.findMin group) a /= -1
      ]

    -- Identyfikator nowego stanu startowego
    newStart = stateMap Map.! findGroup (startState dfa) groups

    -- Nowe stany akceptujące
    newAccepts = Set.fromList
      [ sid
      | g <- groups
      , let sid = stateMap Map.! g
      , not $ Set.null $ Set.intersection g (acceptStates dfa)
      ]

  in DFA
      { states       = Set.fromList (Map.elems stateMap)
      , alphabet     = alphabet dfa
      , transition   = Map.fromList newTransitions
      , startState   = newStart
      , acceptStates = newAccepts
      }