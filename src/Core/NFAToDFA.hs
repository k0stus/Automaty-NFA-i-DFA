-- file   = NFAToDFA.hs
-- author = Tomasz Stefaniak

module Core.NFAToDFA (nfaToDFA) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe (fromMaybe)

import qualified Auto.DFA as DFA
import qualified Auto.NFA as NFA
import Auto.NFA (State, Symbol)

-- Konwertuje NFA na równoważny DFA
nfaToDFA :: NFA.NFA -> DFA.DFA
nfaToDFA nfa = DFA.DFA
  { DFA.states = dfaStates
  , DFA.alphabet = NFA.alphabet nfa
  , DFA.transition = dfaTrans
  , DFA.startState = initialState
  , DFA.acceptStates = accepting
  }
  where
    -- Obliczamy początkowy stan DFA jako epsilon-domknięcie stanu startowego NFA
    initClosure = NFA.epsilonClosure nfa (Set.singleton (NFA.startState nfa))
    
    -- Inicjalizujemy struktury danych
    initialState = 0 -- Nadajemy pierwszy indeks dla stanu startowego DFA
    stateMapping = Map.singleton initClosure initialState -- Mapowanie zbiorów do indeksów
    (dfaStates, dfaTrans, accepting) = 
      buildDFA nfa [initClosure] stateMapping Map.empty Set.empty 1

    -- Główna funkcja budująca DFA (BFS)
    buildDFA :: NFA.NFA 
            -> [Set State] -- Kolejka stanów do przetworzenia
            -> Map (Set State) Int -- Mapowanie zbiorów na indeksy DFA
            -> DFA.Transition -- Akumulowane przejścia DFA
            -> Set State -- Stany akceptujące DFA
            -> Int -- Następny wolny indeks
            -> (Set State, DFA.Transition, Set State)
    buildDFA _ [] _ trans accept _ = (Set.fromList (Map.elems stateMapping), trans, accept)
    
    buildDFA nfa (currentSet:rest) stateMap trans accept nextId = 
      let 
          -- Funkcja przetwarzająca symbol i aktualizująca dostępne stany
          processSymbol sym = 
            let 
                -- Oblicz stany osiągalne przez symbol dla currSet
                targets = Set.unions
                  [ fromMaybe Set.empty (Map.lookup (s, Just sym) (NFA.transition nfa)) -- fromMaybe na wypadek braku przejścia
                  | s <- Set.toList currentSet ]
                
                -- Oblicz domknięcie epsilon, czyli stany osiągalne (kandydat do zostania nowym stanem DFA)
                closure = NFA.epsilonClosure nfa targets
                
                -- Znajdź lub dodaj nowy stan DFA
                (newStateId, newNextId, newStateMap) = 
                  case Map.lookup closure stateMap of
                    Just idx -> (idx, nextId, stateMap) -- Stan już istnieje
                    Nothing -> (nextId, nextId + 1, Map.insert closure nextId stateMap) -- Stan do tej pory nie istaniał
                
                -- Dodaj przejście do DFA z obecnego stanu do nowego stanu (pod wpływem symbolu sym)
                currentStateId = stateMap Map.! currentSet -- Wiemy że currentSet jest w stateMap
                newTrans = Map.insert (currentStateId, sym) newStateId trans
                
                -- Sprawdź czy nowy stan jest akceptujący, jeśli tak, dodaj go do zbioru akceptujących stanów w DFA
                isAccepting = not $ Set.null $ Set.intersection closure (NFA.acceptStates nfa)
                newAccept = if isAccepting then Set.insert newStateId accept else accept
                
            in (closure, newStateId, newTrans, newAccept, newNextId, newStateMap)
          
          -- Przetwarzamy wszystkie symbole alfabetu
          symbolResults = map processSymbol (Set.toList (NFA.alphabet nfa))
          
          -- Zbieramy wyniki
          newStates = [s | (s, _, _, _, _, _) <- symbolResults, not (Map.member s newStateMap)]
          newStateMap = foldl (\m (_, _, _, _, _, nm) -> Map.union m nm) stateMap symbolResults
          newTrans = foldl (\t (_, _, nt, _, _, _) -> nt) trans symbolResults
          newAccept = foldl (\a (_, _, _, na, _, _) -> na) accept symbolResults
          maxNextId = maximum [ni | (_, _, _, _, ni, _) <- symbolResults]
          
      in buildDFA nfa (rest ++ newStates) newStateMap newTrans newAccept maxNextId
