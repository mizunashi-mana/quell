{-# LANGUAGE TemplateHaskell #-}

module Language.Quell.Parsing.Lexer.Lexing.CharEsc where

import qualified Language.Quell.Parsing.Lexer.Rules    as Rules


$(Rules.buildCharEscLexer)
