-- file   = ParseNFA.hs
-- author = kansas

module Utils.ParseNFA (readNFAFromFile) where

import Auto.NFA
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char (isSpace)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)

readNFAFromFile :: FilePath -> IO NFA
readNFAFromFile path = do
  contents <- readFile path
  let ls = filter (not . null) . map strip $ lines contents
  case ls of
    (stateLine : alphaLine : startLine : acceptLine : rest) -> do
      let states = read stateLine :: [Int]
          alphabet = Set.fromList (read alphaLine :: String)
          start = read startLine :: Int
          acceptStates = read acceptLine :: [Int]

      transitions <- foldM parseLine Map.empty rest
      return NFA
        { states = Set.fromList states
        , alphabet = alphabet
        , startState = start
        , acceptStates = Set.fromList acceptStates
        , transition = transitions
        }

    _ -> error "Błąd parsowania pliku: za mało linii wejściowych."

  where
    strip = f . f
      where f = reverse . dropWhile isSpace

    parseLine :: Map.Map (State, Maybe Symbol) (Set.Set State) 
                -> String 
                -> IO (Map.Map (State, Maybe Symbol) (Set.Set State))
    parseLine acc line =
      case words line of
        [s1, "ε", s2]   -> return $ Map.insertWith Set.union (read s1, Nothing) (Set.singleton (read s2)) acc
        [s1, [sym], s2] -> return $ Map.insertWith Set.union (read s1, Just sym) (Set.singleton (read s2)) acc
        _               -> error $ "Nieprawidłowa linia przejścia: " ++ line
