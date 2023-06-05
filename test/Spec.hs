{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Regreg.FA
import Regreg.Parser (regexP)
import Regreg.Algo
import Regreg.Accepts

import Data.IntSet (IntSet)

import qualified Data.Text as T
import Text.Megaparsec
import Data.Maybe (fromJust)


regexToDfa :: T.Text -> DFA IntSet
regexToDfa = nfaToDfa . enfaToNFA . regexToENFA . textToRegex
  where
    textToRegex t = fromJust (parseMaybe regexP t)
  
main :: IO ()
main = hspec $ do
  describe "Regexp check" $ do
    it "works on ``" $ do
      let dfa = regexToDfa ""
      accepts dfa "" `shouldBe` True

      accepts dfa "a" `shouldBe` False
      accepts dfa "ab" `shouldBe` False
      accepts dfa "abc" `shouldBe` False
      
    it "works on `a`" $ do
      let dfa = regexToDfa "a"
      accepts dfa "a" `shouldBe` True

      accepts dfa "ab" `shouldBe` False
      accepts dfa "aa" `shouldBe` False
      accepts dfa "b" `shouldBe` False

    it "works on `a|b`" $ do
      let dfa = regexToDfa "a|b"
      accepts dfa "a" `shouldBe` True
      accepts dfa "b" `shouldBe` True

      accepts dfa "ab" `shouldBe` False
      accepts dfa "ba" `shouldBe` False
      accepts dfa "aa" `shouldBe` False
      accepts dfa "bb" `shouldBe` False

    it "works on `(a|b)*`" $ do
      let dfa = regexToDfa "(a|b)*"
      accepts dfa "" `shouldBe` True
      accepts dfa "a" `shouldBe` True
      accepts dfa "b" `shouldBe` True
      accepts dfa "ab" `shouldBe` True
      accepts dfa "ba" `shouldBe` True
      accepts dfa "aa" `shouldBe` True
      accepts dfa "bb" `shouldBe` True
      accepts dfa "aaa" `shouldBe` True
      accepts dfa "aba" `shouldBe` True

      accepts dfa "abc" `shouldBe` False

    it "works on `a(b|c)*d`" $ do
      let dfa = regexToDfa "a(b|c)*d"
      accepts dfa "ad" `shouldBe` True
      accepts dfa "abd" `shouldBe` True
      accepts dfa "acd" `shouldBe` True
      accepts dfa "abbd" `shouldBe` True
      accepts dfa "abcd" `shouldBe` True
      accepts dfa "accd" `shouldBe` True
      accepts dfa "acbd" `shouldBe` True
      accepts dfa "abbbd" `shouldBe` True
      accepts dfa "acccd" `shouldBe` True

      accepts dfa "a" `shouldBe` False
      accepts dfa "b" `shouldBe` False
      accepts dfa "c" `shouldBe` False
      accepts dfa "d" `shouldBe` False
      accepts dfa "ab" `shouldBe` False
      accepts dfa "ac" `shouldBe` False
      accepts dfa "aad" `shouldBe` False
