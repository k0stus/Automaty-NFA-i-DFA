# Projekt: Operacje na automatach skończonych (NFA → DFA, minimalizacja DFA)

#### Autorzy: Kanstantsin Sasnouski, Tomasz Stefaniak

## Spis treści
- [Projekt: Operacje na automatów skończonych (NFA → DFA, minimalizacja DFA)](#projekt-operacje-na-automatów-skończonych-nfa--dfa-minimalizacja-dfa)
      - [Autorzy: Kanstantsin Sasnouski, Tomasz Stefaniak](#autorzy-kanstantsin-sasnouski-tomasz-stefaniak)
  - [Spis treści](#spis-treści)
  - [Wprowadzenie](#wprowadzenie)
  - [Definicje formalne](#definicje-formalne)
    - [NFA](#nfa)
    - [DFA](#dfa)
  - [Determinizacja](#determinizacja)
    - [Opis algorytmu (subset construction)](#opis-algorytmu-subset-construction)
    - [Wzory](#wzory)
  - [Minimalizacja DFA](#minimalizacja-dfa)
    - [Opis algorytmu (refinement partitioning)](#opis-algorytmu-refinement-partitioning)
    - [Wzory](#wzory-1)
  - [Opis implementacji](#opis-implementacji)
    - [Struktury danych](#struktury-danych)
    - [Moduły](#moduły)
  - [Testowanie](#testowanie)

---

## Wprowadzenie

Celem projektu jest zaimplementowanie operacji na automatach skończonych, w szczególności:
- przekształcenie automatu niedeterministycznego (NFA) w deterministyczny (DFA),
- minimalizacja DFA w celu uzyskania automatu o najmniejszej liczbie stanów rozpoznającego ten sam język.

Implementacja jest napisana w języku Haskell, z wykorzystaniem bibliotek `containers` (dla `Map`, `Set`) i `hspec` (dla testów).

---

## Definicje formalne

### NFA

Niedeterministyczny automat skończony to piątka:

**NFA = (Q, Σ, δ, q₀, F)**, gdzie:
- `Q` – skończony zbiór stanów,
- `Σ` – alfabet wejściowy,
- `δ: Q × (Σ ∪ {ε}) → P(Q)` – funkcja przejścia,
- `q₀ ∈ Q` – stan początkowy,
- `F ⊆ Q` – zbiór stanów akceptujących.

Możliwe są:
- wiele przejść dla danego symbolu,
- ε-przejścia (przejścia bez czytania symbolu).

#### Implementacja

```hs
data NFA = NFA
  { states       :: Set State
  , alphabet     :: Set Symbol
  , transition   :: Transition
  , startState   :: State
  , acceptStates :: Set State
  }
```
gdzie 
- `type State = Int`
- `type Symbol = Char`
- `type Transition = Map (State, Symbol) State`

---

### DFA

Deterministyczny automat skończony to piątka:

**DFA = (Q, Σ, δ, q₀, F)**, gdzie:
- `Q` – skończony zbiór stanów,
- `Σ` – alfabet wejściowy,
- `δ: Q × Σ → Q` – deterministyczna funkcja przejścia,
- `q₀ ∈ Q` – stan początkowy,
- `F ⊆ Q` – zbiór stanów akceptujących.

Brak ε-przejść i brak wieloznaczności w przejściach.

```hs
data DFA = DFA
  { states       :: Set State
  , alphabet     :: Set Symbol
  , transition   :: Transition
  , startState   :: State
  , acceptStates :: Set State
  }
```

---

## Determinizacja

### Opis algorytmu (subset construction)

Zbiór stanów DFA powstaje jako zbiór wszystkich możliwych zbiorów stanów NFA.

1. Stan początkowy DFA to ε-domknięcie stanu początkowego NFA.
2. Dla każdego zbioru `S ⊆ Q` i symbolu `a ∈ Σ`:
   - Obliczamy zbiór `T` wszystkich stanów osiągalnych z `S` przez `a`, z uwzględnieniem ε-przejść.
   - Dodajemy `T` jako nowy stan DFA (jeśli jeszcze nie istnieje).
3. Stan DFA jest akceptujący jeśli zawiera co najmniej jeden stan akceptujący z NFA.

### Wzory

- **ε-closure(q)** = najmniejszy zbiór zawierający `q` oraz wszystkie stany osiągalne przez ε-przejścia.
- **move(S, a)** = zbiór stanów osiągalnych z `S` przez symbol `a`.

---

## Minimalizacja DFA

### Opis algorytmu (refinement partitioning)

Celem jest połączenie stanów, które są nierozróżnialne.

1. Dzielimy stany na dwie klasy: akceptujące i nieakceptujące.
2. Dla każdej klasy sprawdzamy, czy stany mają takie same przejścia dla każdego symbolu.
3. Jeśli nie – dzielimy klasę na mniejsze podklasy.
4. Proces powtarzamy aż do uzyskania stabilnego podziału.

### Wzory

Dwa stany `p, q ∈ Q` są nierozróżnialne, jeśli:
- Dla każdego słowa `w ∈ Σ*`, `δ(p, w) ∈ F ⇔ δ(q, w) ∈ F`.

---

## Opis implementacji

### Moduły

- `Auto.NFA` – definicja i obsługa NFA
- `Auto.DFA` – definicja i obsługa DFA
- `Core.NFAtoDFA` – funkcja `nfaToDFA :: NFA -> DFA`
- `Core.MinimizeDFA` – funkcja `minimizeDFA :: DFA -> DFA`

---

## Testowanie

Testy znajdują się w katalogu `tests/`, zorganizowane jako:

- `NFATest.hs` – testy akceptacji słów przez NFA
- `DFATest.hs` – testy działania deterministycznego automatu
- `minTest.hs` – test poprawności minimalizacji DFA

Uruchamianie testów:

```bash
cabal test
```

Uruchamianie projektu:

```bash
cabal run project -- <option>
```
`Options:` 
- random \<liczba stanów> \<alfabet>
- file \<nazwa pliku>