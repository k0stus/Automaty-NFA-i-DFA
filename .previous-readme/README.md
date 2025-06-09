
#### Autorzy: Kanstantsin Sasnouski, Tomasz Stefaniak

## Automaty skończone - opis projektu
Zaimplementowanie w języku Haskell niedeterministycznego automatu skończonego (NFA),</br>
następnie stworzenie procedury przekształcenia go do DFA oraz algorytmu</br>
minimalizującego liczbę stanów DFA. Zakładamy, że przestrzenią stanów jest skończony</br>
podzbiór liczb naturalnych, natomiast alfabet wejściowy jest z góry określony i skończony.

## Kompilacja i uruchamianie
    - Kompilacja: 'scripts/build.sh' lub po prostu 'cabal build'</br>
    - Uruchomienie głównego programu: 'scripts/execute.sh'</br>
    - Uruchomienie wszystkich testów: 'scripts/test.sh'</br>
    - Uruchomienie pojedyńczego testu: 'cabal repl {nazwa-testu}'</br>

## Szkic projektu rozwiązania
- Zdefiniowanie typów danych dla stanów, alfabetu, funcji przejść oraz reprezentacji automatów
- Implementacja parsera wczytującego reprezentację automatu z pliku tekstowego (lub z konsoli)</br> 
wraz z walidacją danych wejściowych
- Implementacja funkcji symulujących działanie automatów
- Implementacja procedury determinizacji NFA -> DFA
- Implementacja algorytmu minimalizacji automatu
- Dodanie wyświetlania automatów w formacie tekstowym w kluczowych etapach działania
- Testy

## Wstępny podział prac
#### Kanstantsin:
- Implementacja NFA oraz testów
- Implementacja funkcji minimalizującej DFA oraz testów
- Implementacja parsera NFA

#### Tomasz: 
- Implementacja DFA oraz testów do niego
- Implementacja funkcji konwertującej NFA na DFA oraz testów
- Implementacja generatora NFA

## Wykorzystywane biblioteki
- Prelude (Podstawowe typy i funkcje)
- System.IO (Obsługa wejścia i wyjścia, np. by łatwo wczytywać różne automaty)
- HLint (Zadbanie o czysty kod)
- HSpec (Testy jednostkowe)
