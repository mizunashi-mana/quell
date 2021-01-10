
{-# LANGUAGE TemplateHaskell  #-}

module Language.Quell.Parsing.Lexer (
  LexerContext (..),
  LayoutContext (..),
) where

import Language.Quell.Prelude

import qualified Language.Quell.Parsing.Lexer.Rules as LexerRules
import qualified Language.Quell.Type.Token as Token


$(LexerRules.buildLexer)

data LexerContext = LexerContext
  {
    restBuffer :: ByteString,
    currentPosition :: Int,
    lastToken :: Maybe Token.T,
    layoutContextStack :: [LayoutContext]
  }
  deriving (Eq, Show)

data LayoutContext
  = NoLayout
  | CurlyLayout Int
  | DCurlyLayout Int
  deriving (Eq, Show)


