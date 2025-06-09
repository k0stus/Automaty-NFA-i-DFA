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
import Auto.NFA (State, Symbol)

-- Funkcja minimalizacji deterministycznego automatu skończonego (DFA)
minimizeDFA :: DFA -> DFA
minimizeDFA dfa =
  let
    -- 1. Inicjalny podział: stany akceptujące i nieakceptujące
    initialPartition = [acceptStates dfa, Set.difference (states dfa) (acceptStates dfa)]

    -- 2. Rekurencyjnie rafinujemy grupy, aż podział się ustabilizuje
    refine :: [Set State] -> [Set State]
    refine groups =
      let groups' = refineOnce groups
      in if groups' == groups then groups else refine groups'

    refineOnce :: [Set State] -> [Set State]
    refineOnce groups = concatMap (splitGroup groups) groups

    -- 3. Dzieli jedną grupę stanów na podgrupy, zgodnie z ich zachowaniem dla każdego symbolu
    splitGroup :: [Set State] -> Set State -> [Set State]
    splitGroup groups group =
      let classify state =
            [ if t == -1 then Nothing else Just (findGroup t groups)
            | a <- Set.toList (alphabet dfa)
            , let t = move state a
            ]
          buckets = Map.fromListWith Set.union
                      [ (classify s, Set.singleton s) | s <- Set.toList group ]
      in Map.elems buckets

    -- 4. Znajduje grupę, do której należy dany stan
    findGroup :: State -> [Set State] -> Set State
    findGroup s = fromJust . find (Set.member s)

    -- 5. Funkcja przejścia: -1 oznacza "dead state"
    move :: State -> Symbol -> State
    move s a = Map.findWithDefault (-1) (s, a) (transition dfa)

    -- 6. Finalne grupy po stabilizacji podziału
    groups = refine initialPartition

    -- 7. Przypisujemy każdej grupie unikalny numer (nowy stan)
    stateMap :: Map (Set State) State
    stateMap = Map.fromList $ zip groups [0..]

    -- 8. Budujemy nowe przejścia tylko dla tych, które nie prowadzą do dead state (-1)
    newTransitions =
      [ ((stateMap Map.! group, a), stateMap Map.! findGroup t groups)
      | group <- groups
      , a <- Set.toList (alphabet dfa)
      , let t = move (Set.findMin group) a
      , t /= -1
      ]

    -- 9. Nowy stan początkowy
    newStart = stateMap Map.! findGroup (startState dfa) groups

    -- 10. Nowe stany akceptujące — jeśli jakakolwiek część grupy była akceptująca
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