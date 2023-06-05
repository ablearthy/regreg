module Regreg.Algo (regexToENFA, enfaToNFA, nfaToDfa) where

import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Regreg.FA
import Regreg.Regexp

regexToENFA :: Regexp -> ENFA
regexToENFA r = evalState (regexToENFA' r) 0

regexToENFA' :: Regexp -> State Int ENFA
regexToENFA' RegEmpty = do
    sno <- get
    modify (+ 1)
    pure $
        ENFA
            { enfaGraph = IntMap.singleton sno Map.empty
            , epsGraph = IntMap.empty
            , enfaInitial = sno
            , enfaTerminating = sno
            }
regexToENFA' (RegChar ch) = do
    sno <- get
    modify (+ 2)
    pure $
        ENFA
            { enfaGraph = IntMap.singleton sno $ Map.singleton ch $ IntSet.singleton $ sno + 1
            , epsGraph = IntMap.empty
            , enfaInitial = sno
            , enfaTerminating = sno + 1
            }
regexToENFA' (RegUnion a b) = do
    enfa1 <- regexToENFA' a
    enfa2 <- regexToENFA' b
    sno <- get
    modify (+ 2)
    let start1 = enfaInitial enfa1
    let start2 = enfaInitial enfa2
    let end1 = enfaTerminating enfa1
    let end2 = enfaTerminating enfa2
    pure $
        ENFA
            { enfaGraph = enfaGraph enfa1 `IntMap.union` enfaGraph enfa2
            , epsGraph =
                IntMap.fromList
                    [ (sno, IntSet.fromList [start1, start2])
                    , (end1, IntSet.singleton (sno + 1))
                    , (end2, IntSet.singleton (sno + 1))
                    ]
                    `IntMap.union` epsGraph enfa1
                    `IntMap.union` epsGraph enfa2
            , enfaInitial = sno
            , enfaTerminating = sno + 1
            }
regexToENFA' (RegConcat a b) = do
    enfa1 <- regexToENFA' a
    enfa2 <- regexToENFA' b
    modify (+ 1)
    let start1 = enfaInitial enfa1
    let start2 = enfaInitial enfa2
    let end1 = enfaTerminating enfa1
    let end2 = enfaTerminating enfa2
    pure $
        ENFA
            { enfaGraph = enfaGraph enfa1 `IntMap.union` enfaGraph enfa2
            , epsGraph =
                IntMap.singleton end1 (IntSet.singleton start2)
                    `IntMap.union` epsGraph enfa1
                    `IntMap.union` epsGraph enfa2
            , enfaInitial = start1
            , enfaTerminating = end2
            }
regexToENFA' (RegKleene a) = do
    enfa <- regexToENFA' a
    sno <- get
    modify (+ 2)
    let start = enfaInitial enfa
    let end = enfaTerminating enfa
    pure $
        ENFA
            { enfaGraph = enfaGraph enfa
            , epsGraph =
                IntMap.fromList
                    [ (sno, IntSet.fromList [sno + 1, start])
                    , (end, IntSet.fromList [sno + 1, start])
                    ]
                    `IntMap.union` epsGraph enfa
            , enfaInitial = sno
            , enfaTerminating = sno + 1
            }

enfaToNFA :: ENFA -> NFA
enfaToNFA enfa = NFA{nfaGraph = newTransitionFunction, nfaInitial = enfaInitial enfa, nfaTerminating = terminatingStates}
  where
    newTransitionFunction = foldl' (\acc _ -> propagateChars acc (epsGraph enfa)) (enfaGraph enfa) [0 .. size]
    terminatingStates = foldl' (\acc _ -> getTerminalStates acc (epsGraph enfa)) (IntSet.singleton (enfaTerminating enfa)) [0 .. size]
    size = IntMap.size $ epsGraph enfa

propagateChars :: Graph IntSet.IntSet -> EGraph -> Graph IntSet.IntSet
propagateChars = IntMap.foldlWithKey' go
  where
    go :: Graph IntSet -> Int -> IntSet.IntSet -> Graph IntSet
    go graph from = IntSet.foldl' (\acc to -> go' acc from to) graph

    go' :: Graph IntSet -> Int -> Int -> Graph IntSet
    go' graph from to =
        Map.foldlWithKey'
            ( \acc ch set ->
                IntSet.foldl' (\acc' s -> addToGraph acc' from ch s) acc set
            )
            graph
            transitions
      where
        transitions = fromMaybe Map.empty $ IntMap.lookup to graph

    addToGraph :: Graph IntSet -> Int -> Char -> Int -> Graph IntSet
    addToGraph graph' a ch b = IntMap.insertWith (Map.unionWith IntSet.union) a node graph'
      where
        node = Map.singleton ch (IntSet.singleton b)

getTerminalStates :: IntSet.IntSet -> EGraph -> IntSet.IntSet
getTerminalStates =
    IntMap.foldlWithKey'
        ( \acc key set ->
            if any (`IntSet.member` acc) (IntSet.toList set)
                then IntSet.insert key acc
                else acc
        )

nfaToDfa :: NFA -> DFA IntSet.IntSet
nfaToDfa nfa =
    DFA
        { dfaGraph = go Map.empty (Set.singleton $ IntSet.singleton $ nfaInitial nfa)
        , dfaInitial = nfaInitial nfa
        , dfaTerminating = nfaTerminating nfa
        }
  where
    getState :: Char -> IntSet -> IntSet
    getState ch = IntSet.foldl' (\acc k -> acc `IntSet.union` transition ch k (nfaGraph nfa)) IntSet.empty

    go :: MapGraph IntSet IntSet -> Set IntSet -> MapGraph IntSet IntSet
    go acc newStates
        | newStates == Set.empty = acc
        | otherwise =
            let (newStates', acc') =
                    Set.foldl'
                        (\(ns', graph) s -> let (a, b) = go' s graph in (ns' `Set.union` a, b))
                        (Set.empty, acc)
                        newStates
             in go acc' (newStates' `Set.difference` newStates)

    go' :: IntSet -> MapGraph IntSet IntSet -> (Set IntSet, MapGraph IntSet IntSet)
    go' s acc =
        Set.foldl'
            (\(newStates, graph) (ch, to) -> (Set.insert to newStates, addToGraph' graph s ch to))
            (Set.empty, acc)
            (go'' s)

    go'' :: IntSet -> Set (Char, IntSet)
    go'' s =
        Set.foldl'
            ( \acc ch ->
                let states = getState ch s
                 in if states == IntSet.empty then acc else Set.insert (ch, states) acc
            )
            Set.empty
            t
      where
        t = IntSet.foldl' (\acc k -> acc `Set.union` getCharsFromGraph k (nfaGraph nfa)) Set.empty s

    addToGraph' :: MapGraph IntSet IntSet -> IntSet -> Char -> IntSet -> MapGraph IntSet IntSet
    addToGraph' graph' a ch b = Map.insertWith Map.union a (Map.singleton ch b) graph'
