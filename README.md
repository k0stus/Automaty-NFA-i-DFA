
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

## Opis funkcji konwertującej NFA do DFA
NFA można przekonwertować do DFA (konwersja na zaimplementowanych typach)</br>
przez tzw. subset construction. W zaimplementowanej funkcji:
- Stanami DFA są epsilon-domknięte zbiory stanów NFA, osiągalne poprzez przejścia symbolami z alfabetu.
- Stan początkowy DFA to epsilon-domknięcie stanu początkowego NFA.
- Przejścia w DFA odpowiadają przejściom między zbiorami stanów NFA po danym symbolu, z dodatkowym epsilon-domknięciem.
- Stany akceptujące DFA to te, których reprezentowane zbiory zawierają przynajmniej jeden</br> stan akceptujący NFA (nie muszą to być wszystkie stany, jak błędnie napisałeś).

Funkcja działa w sposób BFS:
- Startuje od początkowego zbioru (domknięcia epsilonowe stanu początkowego).
- Dla każdego zbioru z kolejki i każdego symbolu alfabetu oblicza, dokąd można przejść.
- Jeśli nowy zbiór nie był jeszcze odwiedzony, przypisuje mu nowy indeks stanu DFA.
- Dodaje odpowiednie przejścia i aktualizuje zbiór stanów akceptujących.
- Kończy działanie, gdy kolejka zbiorów do odwiedzenia jest pusta.

Jej złożoność to Θ(liczba unikalnych osiągalnych zbiorów stanów w NFA)

## Opis funkcji minimalizującej DFA

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
- Implementacja NFA oraz testów do niego
- Implementacja funkcji minimalizującej DFA oraz testów do niego
-

#### Tomasz: 
- Implementacja DFA oraz testów do niego
- Implementacja funkcji konwertującej NFA na DFA
-

#### Wspólna praca (ciężkie z góry do podziału):
- Implementacja algorytmu minimalizującego DFA
- Dodanie wyświetlania automatów w istotnych momentach
- 

## Wykorzystywane biblioteki
- Prelude (Podstawowe typy i funkcje)
- System.IO (Obsługa wejścia i wyjścia, np. by łatwo wczytywać różne automaty)
- HLint (Zadbanie o czysty kod)
- HSpec (Testy jednostkowe)
