-- file   = GenerateRandomNFA.hs
-- author = Tomasz Stefaniak

module Utils.GenerateRandomNFA (generateRandomNFA) where

import System.Random (randomRIO)
import Control.Monad (replicateM)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import Data.Maybe (catMaybes)

import Auto.NFA

generateRandomNFA :: Int -> Set Symbol -> IO NFA
generateRandomNFA numStates alphabet = do
  let statesSet = Set.fromList [0 .. numStates - 1]

  -- Generowanie zwykłych przejść
  transList <- fmap catMaybes . sequence $
    [ do
        include <- randomRIO (False, True) -- Czy dodać przejście (dla uwzględniania braku przejść)
        if not include
          then pure Nothing
          else do
            n <- randomRIO (1, 3) -- Liczba przejść dla danego stanu i symbolu
            targets <- replicateM n (randomRIO (0, numStates - 1))
            pure $ Just ((state, Just sym), Set.fromList targets)
    | state <- [0 .. numStates - 1]
    , sym <- Set.toList alphabet
    ]

  -- Generowanie epsilon-przejść
  epsList <- sequence
    [ do
        k <- randomRIO (0, 2) -- Liczba epsilon-przejść dla danego stanu
        if k == 0
          then pure Nothing
          else do
            targets <- replicateM k (randomRIO (0, numStates - 1))
            pure $ Just ((state, Nothing), Set.fromList targets)
    | state <- [0 .. numStates - 1]
    ]

  let mergeTransitions = Map.fromListWith Set.union
  let transMap = mergeTransitions (transList ++ catMaybes epsList)

  -- Stan początkowy
  let start = 0

  -- Stany akceptujące
  let allStates = [0 .. numStates - 1]
  numAccept <- randomRIO (1, max 1 (numStates `div` 5)) -- Co najmniej jeden stan akceptujący
  acceptStatesList <- replicateM numAccept (randomRIO (0, numStates - 1))
  let acceptSet = Set.fromList acceptStatesList

  pure NFA
    { states = statesSet
    , alphabet = alphabet
    , transition = transMap
    , startState = start
    , acceptStates = acceptSet
    }
