-- file   = NFAToDFA.hs
-- author = Tomasz Stefaniak

module Core.NFAToDFA (nfaToDFA) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe (fromMaybe)
import Debug.Trace

import qualified Auto.DFA as DFA
import qualified Auto.NFA as NFA
import Auto.NFA (State, Symbol)

-- Konwertuje NFA na równoważny DFA
nfaToDFA :: NFA.NFA -> DFA.DFA
nfaToDFA nfa = DFA.DFA
  { DFA.states = Set.insert trapStateId dfaStates
  , DFA.alphabet = NFA.alphabet nfa
  , DFA.transition = dfaTrans
  , DFA.startState = initialState
  , DFA.acceptStates = accepting
  }
  where
    -- Zmienna od indeksu tzw stanu pułapkowego, który oznacza pustą listę stanów z NFA
    trapStateId = -1
    -- Obliczamy początkowy stan DFA jako epsilon-domknięcie stanu startowego NFA
    -- rawInitClosure = NFA.epsilonClosure nfa (Set.singleton (NFA.startState nfa))
    -- initClosure = trace ("Init closure: " ++ show rawInitClosure) rawInitClosure
    initClosure = NFA.epsilonClosure nfa (Set.singleton (NFA.startState nfa))
    
    -- Inicjalizujemy struktury danych
    initialState = 0 -- Nadajemy pierwszy indeks dla stanu startowego DFA
    stateMapping = Map.fromList [(initClosure, initialState), (Set.empty, trapStateId)] -- Mapowanie zbiorów do indeksów
    
    -- Sprawdzamy, czy stan początkowy jest akceptujący 
    initialAccept = if Set.null (Set.intersection initClosure (NFA.acceptStates nfa))
                    then Set.empty
                    else Set.singleton initialState
    
    (dfaStates, dfaTrans, accepting) = 
      buildDFA nfa [initClosure] stateMapping Map.empty initialAccept 1

    -- Główna funkcja budująca DFA (BFS)
    buildDFA :: NFA.NFA 
            -> [Set State] -- Kolejka stanów do przetworzenia
            -> Map (Set State) Int -- Mapowanie zbiorów na indeksy DFA
            -> DFA.Transition -- Akumulowane przejścia DFA
            -> Set State -- Stany akceptujące DFA
            -> Int -- Następny wolny indeks
            -> (Set State, DFA.Transition, Set State)
    buildDFA _ [] stateMap trans accept _ = (Set.fromList (Map.elems stateMap), trans, accept)
    
    buildDFA nfa (currentSet:rest) stateMap trans accept nextId = 
      let 
          currentStateId = stateMap Map.! currentSet
          -- Log the current set being processed, tied to alphabetList evaluation
          -- alphabetList = trace ("Processing DFA state (from NFA states): " ++ show currentSet) (Set.toList (NFA.alphabet nfa))
          alphabetList = Set.toList (NFA.alphabet nfa)

          -- Fold over symbols, accumulating new transitions, new states to explore (as NFA state sets),
          -- updated stateMap, the next available ID, and updated accepting states set.
          foldFunction (accTrans, accNewStatesToExplore, accAcceptingStates, accStateMap, accNextId) sym =
            let 
                -- Oblicz stany osiągalne przez symbol dla currSet
                targets = Set.unions
                  [ fromMaybe Set.empty (Map.lookup (s, Just sym) (NFA.transition nfa))
                  | s <- Set.toList currentSet ]
                
                -- Oblicz domknięcie epsilon, czyli stany osiągalne (kandydat do zostania nowym stanem DFA)
                -- Ensure trace is evaluated by making it return the closure value.
                -- rawClosure = if Set.null targets 
                --              then Set.empty -- specjalny zbiór dla stanu pułapkowego
                --              else NFA.epsilonClosure nfa targets
                -- closure = trace ("  .. Symbol '" ++ show sym ++ "' -> Closure: " ++ show rawClosure) rawClosure
                closure = if Set.null targets 
                          then Set.empty -- specjalny zbiór dla stanu pułapkowego
                          else NFA.epsilonClosure nfa targets
                
                -- Znajdź lub dodaj nowy stan DFA
                (newStateId, finalNextIdAfterSymbol, finalStateMapAfterSymbol, isNewState) =
                  if Set.null closure
                  then (trapStateId, accNextId, accStateMap, False) -- Stan pułapkowy
                  else case Map.lookup closure accStateMap of
                    Just idx -> (idx, accNextId, accStateMap, False) -- Stan już istnieje
                    Nothing  -> (accNextId, accNextId + 1, Map.insert closure accNextId accStateMap, True) -- Stan do tej pory nie istniał

                -- Dodaj przejście do DFA z obecnego stanu do nowego stanu (pod wpływem symbolu sym)
                updatedTrans = Map.insert (currentStateId, sym) newStateId accTrans
                
                -- Sprawdź czy nowy/docelowy stan DFA (reprezentowany przez 'closure') jest akceptujący
                isAccepting = not (Set.null (Set.intersection closure (NFA.acceptStates nfa)))
      
                -- Log if the state is accepted by embedding trace in the expression
                updatedAcceptingStates = 
                  if Set.null closure -- Stan pułapkowy
                  then accAcceptingStates -- Zostaw bez zmian
                  else if isAccepting
                      -- then trace ("    .. DFA state " ++ show newStateId ++ " (from NFA states: " ++ show closure ++ ") is ACCEPTED") (Set.insert newStateId accAcceptingStates)
                      then Set.insert newStateId accAcceptingStates
                      else accAcceptingStates
                
                -- Dodaj nowy stan (closure) do kolejki do przetworzenia, jeśli jest nowy i niepusty
                updatedNewStatesToExplore = if isNewState && not (Set.null closure) 
                                            then closure : accNewStatesToExplore 
                                            else accNewStatesToExplore
                
            in (updatedTrans, updatedNewStatesToExplore, updatedAcceptingStates, finalStateMapAfterSymbol, finalNextIdAfterSymbol)
          
          -- Inicjalizujemy wartości dla folda.
          -- Przejścia (`trans`), stany akceptujące (`accept`), `stateMap` i `nextId` są przekazywane z poprzednich kroków budowy DFA.
          -- `newStatesToExploreForCurrentSet` jest pusta na początku przetwarzania symboli dla `currentSet`.
          initialFoldAccumulator = (trans, [], accept, stateMap, nextId)
          
          (processedTrans, newStatesForQueueReversed, processedAcceptingStates, processedStateMap, processedNextId) =
             foldl foldFunction initialFoldAccumulator alphabetList
          
          -- Odwracamy listę nowych stanów, aby zachować kolejność odkrywania
          newStatesForQueue = reverse newStatesForQueueReversed
          
      in buildDFA nfa (rest ++ newStatesForQueue) processedStateMap processedTrans processedAcceptingStates processedNextId
