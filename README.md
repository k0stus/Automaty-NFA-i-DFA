# Automaty-NFA-i-DFA
Project for Functional Programming course at PWr(Wrocław University of Science &amp; Technology)
## Opis ogólny projektu
Celem projektu jest zaimplementowanie w języku Haskell modelu niedeterministycznego automatu skończonego (NFA), a następnie stworzenie procedur jego determinizacji (przekształcenie do DFA) oraz minimalizacji liczby stanów DFA. Zakładamy, że przestrzenią stanów są liczby naturalne, a alfabet wejściowy jest z góry określony i skończony.
## Komponenty rozwiązania
- Zdefiniowanie typów danych dla stanów, alfabetu, funcji przejść oraz automatu
- Parser dla wczytywania automatu (z pliku tekstowego lub z konsoli)
- Determinizacja NFA -> DFA
- Implementacja algorytmu minimalizacji automatu
## Narzędzia
- Prelude
- System.IO
- hlint
