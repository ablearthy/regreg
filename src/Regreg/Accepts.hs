module Regreg.Accepts (accepts) where

import Regreg.FA (DFA (..))

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import qualified Data.Map.Strict  as Map

import qualified Data.Text as T
import Data.Text.Internal.Fusion
    ( stream, Step(Yield, Done, Skip), Stream(..) )

accepts :: DFA IntSet -> T.Text -> Bool
accepts dfa t = accepts' dfa (stream t)

{-# INLINE accepts #-}

accepts' :: DFA IntSet -> Stream Char -> Bool
accepts' dfa (Stream next0 s0 _len) = go (IntSet.singleton $ dfaInitial dfa) s0
  where
    go curState !s = case next0 s of
      Done -> isTerminal curState
      Skip s' -> go curState s'
      Yield x s' -> case yield x curState of
        Nothing -> False
        Just newState -> go newState s'

    isTerminal :: IntSet -> Bool
    isTerminal s = (dfaTerminating dfa `IntSet.intersection` s) /= IntSet.empty

    yield :: Char -> IntSet -> Maybe IntSet
    yield ch curState = do
      m <- Map.lookup curState (dfaGraph dfa)
      Map.lookup ch m 

{-# INLINE [0] accepts' #-}
