{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Regreg.FA
import Regreg.Algo
import Regreg.Parser
import Regreg.Accepts

import Text.Megaparsec

import Data.Maybe

import Data.IntSet (IntSet)
import qualified Data.IntMap.Strict  as IntMap

import qualified Data.Map.Strict  as Map

import Control.Monad

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.IO

prompt :: T.Text -> IO T.Text
prompt text = do
    TIO.putStr text
    hFlush stdout
    TIO.getLine



printNFA :: Graph IntSet -> IO ()
printNFA graph = forM_ (IntMap.toList graph) $ \(s, m) -> do
  putStr (show s)
  putStr " : "
  print m

printDFA :: DFA IntSet -> IO ()
printDFA dfa = do
  putStrLn $ "Initial state: " <> show (dfaInitial dfa)
  putStrLn $ "Terminal states: " <> show (dfaTerminating dfa)
  forM_ (Map.toList (dfaGraph dfa)) $ \(from, trans) -> do
    forM_ (Map.toList trans) $ \(ch, to) -> do
      putStrLn $ show from <> " -> " <> show ch <> " -> " <> show to


main :: IO ()
main = do
  reg <- prompt "Enter the regexp: "
  let r = fromJust (parseMaybe regexP reg)
  let enfa = regexToENFA r
  let nfa = enfaToNFA enfa
  let dfa = nfaToDfa nfa
  TIO.putStrLn "--8<-- DEBUG --8<--"
  print r
  print enfa
  print nfa
  printNFA (nfaGraph nfa)
  printDFA dfa
  TIO.putStrLn "-->8-- DEBUG -->8--"
  forever $ do
    text <- prompt "Enter the string to match: "
    (if accepts dfa text then putStrLn "OK" else putStrLn "NOT OK")