module Regreg.Regexp (Regexp (..)) where

data Regexp
    = RegEmpty
    | RegChar Char
    | RegUnion Regexp Regexp
    | RegConcat Regexp Regexp
    | RegKleene Regexp
    deriving (Show, Eq)
