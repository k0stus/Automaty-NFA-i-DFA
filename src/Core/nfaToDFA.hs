-- file   = nfaToDFA.hs
-- author = Tomasz Stefaniak

module Core.NFAtoDFA (nfaToDFA) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe (fromMaybe)
import Auto.DFA
import Auto.NFA


-- Konwertuje NFA na równoważny DFA
nfaToDFA :: Auto.NFA -> Auto.DFA
nfaToDFA nfa = Auto.DFA
  { states = dfaStates
  , alphabet = alphabet nfa -- Po prostu kopiujemy alfabet z NFA
  , transition = dfaTrans
  , startState = initialState
  , acceptStates = accepting
  }
  where
    -- Obliczamy początkowy stan DFA jako epsilon-domknięcie stanu startowego NFA
    initClosure = epsilonClosure nfa (Set.singleton (startState nfa))
    
    -- Inicjalizujemy struktury danych
    initialState = 0 -- Nadajemy pierwszy indeks dla stanu startowego DFA
    stateMapping = Map.singleton initClosure initialState -- Mapowanie zbiorów do indeksów
    (dfaStates, dfaTrans, accepting) = 
      buildDFA nfa [initClosure] stateMapping Map.empty Set.empty 1

    -- Główna funkcja budująca DFA (BFS)
    buildDFA :: Auto.NFA 
             -> [Set State]          -- Kolejka stanów do przetworzenia
             -> Map (Set State) Int  -- Mapowanie zbiorów na indeksy DFA
             -> Transition           -- Akumulowane przejścia DFA
             -> Set State            -- Stany akceptujące DFA
             -> Int                  -- Następny wolny indeks
             -> (Set State, Transition, Set State)
    buildDFA _ [] _ trans accept _ = 
      (Set.fromList (Map.elems stateMapping), trans, accept)
    
    buildDFA nfa (currentSet:rest) stateMap trans accept nextId =
      let -- Dla każdego symbolu w alfabecie...
          processSymbol sym =
            -- Oblicz stany osiągalne przez symbol
            let targets = Set.unions
                  [ fromMaybe Set.empty (Map.lookup (s, Just sym) (transition nfa))
                  | s <- Set.toList currentSet ]
                
                -- Oblicz epsilon-domknięcie
                closure = epsilonClosure nfa targets
                
                -- Znajdź lub dodaj nowy stan DFA
                (newStateId, newNextId, newStateMap) = 
                  case Map.lookup closure stateMap of
                    Just idx -> (idx, nextId, stateMap) -- Stan już istnieje
                    Nothing -> (nextId, nextId + 1, Map.insert closure nextId stateMap)
                
                -- Dodaj przejście do DFA
                currentStateId = stateMap Map.! currentSet
                newTrans = Map.insert (currentStateId, sym) newStateId trans
                
                -- Sprawdź czy stan akceptujący
                isAccepting = not $ Set.null $ Set.intersection closure (acceptStates nfa)
                newAccept = if isAccepting then Set.insert newStateId accept else accept
                
            in (closure, newStateId, newTrans, newAccept, newNextId, newStateMap)
          
          -- Przetwarzamy wszystkie symbole alfabetu
          symbolResults = map processSymbol (Set.toList (alphabet nfa))
          
          -- Zbieramy wyniki
          newStates = [s | (s, _, _, _, _, _) <- symbolResults, 
                          not (Map.member s stateMap)]
          newStateMap = foldl (\m (_, _, _, _, _, nm) -> nm) stateMap symbolResults
          newTrans = foldl (\t (_, _, nt, _, _, _) -> nt) trans symbolResults
          newAccept = foldl (\a (_, _, _, na, _, _) -> na) accept symbolResults
          maxNextId = maximum [ni | (_, _, _, _, ni, _) <- symbolResults]
          
      in buildDFA nfa (rest ++ newStates) newStateMap newTrans newAccept maxNextId
