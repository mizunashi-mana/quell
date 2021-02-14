module Language.Quell.Parsing.Parser.Error (
    T,
    Error (..),
) where

import           Language.Quell.Prelude


type T = Error

data Error
    = ParseError
    deriving (Eq, Show)
