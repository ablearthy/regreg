module Regreg.FA (Vertex, Graph, EGraph, MapGraph, ENFA (..), DFA (..), NFA (..), transition, getCharsFromGraph) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe

type Vertex = Int

type Graph a = IntMap (Map.Map Char a)
type MapGraph a b = Map.Map a (Map.Map Char b)

type EGraph = IntMap IntSet

data ENFA = ENFA
    { enfaGraph :: Graph IntSet
    , epsGraph :: EGraph
    , enfaInitial :: Vertex
    , enfaTerminating :: Vertex
    }
    deriving (Show)

data NFA = NFA
    { nfaGraph :: Graph IntSet
    , nfaInitial :: Vertex
    , nfaTerminating :: IntSet
    }
    deriving (Show)

data DFA a = DFA
    { dfaGraph :: Map.Map a (Map.Map Char a)
    , dfaInitial :: Vertex
    , dfaTerminating :: IntSet
    }
    deriving (Show)

transition :: Char -> Int -> Graph IntSet -> IntSet
transition ch v graph = fromMaybe IntSet.empty $ do
    m <- IntMap.lookup v graph
    Map.lookup ch m

getCharsFromGraph :: Int -> Graph IntSet -> Set Char
getCharsFromGraph v graph = maybe Set.empty Map.keysSet (IntMap.lookup v graph)
