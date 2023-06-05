module Regreg.Parser (Regexp (..), regexP, factorP) where

import Data.Text qualified as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char)

import Regreg.Regexp ( Regexp(..) ) 

type Parser = Parsec Void T.Text

baseExprP :: Parser Regexp
baseExprP =
  (char '(' *> regexP <* char ')')
    <|> (RegChar <$> noneOf ['(', ')', '*', '|'])

factorP :: Parser Regexp
factorP = do
  base <- baseExprP
  maybeStar <- optional $ char '*'
  case maybeStar of
    Just _ -> pure $ RegKleene base
    Nothing -> pure base

regexP :: Parser Regexp
regexP = do
  term1 <- termP
  maybeUnion <- optional . try  $ char '|'
  case maybeUnion of
    Just _ -> RegUnion term1 <$> regexP
    Nothing -> pure term1
  where
    termP = do
      lst <- many factorP
      case lst of 
        (h:t) -> pure $ foldl RegConcat h t
        [] -> pure RegEmpty