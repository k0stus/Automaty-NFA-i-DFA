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
  transList <- sequence
    [ do
        n <- randomRIO (1, 2)  -- losowa liczba przejść dla danego (state, symbol)
        targets <- replicateM n (randomRIO (0, numStates - 1))
        pure ((state, Just sym), Set.fromList targets)
    | state <- [0 .. numStates - 1]
    , sym <- Set.toList alphabet
    ]

  -- Generowanie epsilon-przejść (do 3 na stan)
  epsList <- sequence
    [ do
        k <- randomRIO (0, 3)
        if k == 0
          then pure Nothing
          else do
            targets <- replicateM k (randomRIO (0, numStates - 1))
            pure $ Just ((state, Nothing), Set.fromList targets)
    | state <- [0 .. numStates - 1]
    ]

  let transMap = Map.fromList (transList ++ catMaybes epsList)

  -- Stan początkowy
  let start = 0

  -- Stany akceptujące
  let allStates = [0 .. numStates - 1]
  numAccept <- randomRIO (1, max 1 (numStates `div` 3))
  acceptStatesList <- replicateM numAccept (randomRIO (0, numStates - 1))
  let acceptSet = Set.fromList acceptStatesList

  pure NFA
    { states = statesSet
    , alphabet = alphabet
    , transition = transMap
    , startState = start
    , acceptStates = acceptSet
    }
