-- file   = MinimizeDFA
-- author = kansas

module Core.MinimizeDFA (minimizeDFA) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import Data.List (find)
import Data.Maybe (fromMaybe)

import Auto.DFA
import Auto.NFA (State, Symbol)

-- Funkcja minimalizacji deterministycznego automatu skończonego (DFA)
minimizeDFA :: DFA -> DFA
minimizeDFA oldDFA =
  let
    -- Usuń stany martwe
    aliveStateDFA = removeDeadStates oldDFA

    -- Usuń nieosiągalne stany
    reachable = reachableStates aliveStateDFA
    dfa = oldDFA
      { states = reachable
      , transition = Map.filterWithKey (\(s, _) t -> s `Set.member` reachable && t `Set.member` reachable) (transition oldDFA)
      , acceptStates = Set.filter (`Set.member` reachable) (acceptStates oldDFA)
      }

    -- 1. Inicjalny podział: stany akceptujące i nieakceptujące
    initialPartition = filter (not . Set.null)
      [acceptStates dfa, Set.difference (states dfa) (acceptStates dfa)]

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

    -- 4. Znajduje grupę, do której należy dany stan (bezpiecznie)
    findGroup :: State -> [Set State] -> Set State
    findGroup s groups = fromMaybe (error $ "State " ++ show s ++ " not found in any group") $
      find (Set.member s) groups

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
      , not (Set.null group)
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
    
    newDFA = DFA
      { states       = Set.fromList (Map.elems stateMap)
      , alphabet     = alphabet dfa
      , transition   = Map.fromList newTransitions
      , startState   = newStart
      , acceptStates = newAccepts
      }
    
  in newDFA

-- Funkcja pomocnicza: znajduje stany osiągalne od stanu początkowego
reachableStates :: DFA -> Set State
reachableStates dfa = go Set.empty [startState dfa]
  where
    go visited [] = visited
    go visited (s:ss)
      | s `Set.member` visited = go visited ss
      | otherwise =
          let next = [t | ((s', _), t) <- Map.toList (transition dfa), s' == s]
          in go (Set.insert s visited) (next ++ ss)

-- Funkcja pomocnicza: znajduje stany, z których da się dotrzeć do stanu akceptującego
removeDeadStates :: DFA -> DFA
removeDeadStates dfa =
  let
    -- Odwróć graf przejść (dla "wstecznego" DFS)
    reversedTransitions :: Map State [State]
    reversedTransitions = Map.fromListWith (++)
      [ (t, [s]) | ((s, _), t) <- Map.toList (transition dfa) ]

    -- DFS od stanów akceptujących w "tył"
    reachableFromAccepting :: Set State
    reachableFromAccepting = go Set.empty (Set.toList $ acceptStates dfa)
      where
        go visited [] = visited
        go visited (s:ss)
          | s `Set.member` visited = go visited ss
          | otherwise =
              let preds = Map.findWithDefault [] s reversedTransitions
              in go (Set.insert s visited) (preds ++ ss)

    -- Stany, które są zarówno osiągalne od startu, jak i mogą dojść do akceptujących
    liveStates = Set.intersection (reachableStates dfa) reachableFromAccepting

    -- Filtrowanie przejść tylko do i z liveStates
    newTransitions = Map.filterWithKey (\(s, _) t -> s `Set.member` liveStates && t `Set.member` liveStates) (transition dfa)

  in dfa
     { states = liveStates
     , transition = newTransitions
     , acceptStates = Set.filter (`Set.member` liveStates) (acceptStates dfa)
     , startState = startState dfa -- nie zmieniamy — może być i tak już niedostępny, ale wtedy zostanie wycięty
     }