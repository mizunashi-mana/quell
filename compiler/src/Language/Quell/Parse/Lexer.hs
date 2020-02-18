-- |
--
-- TODO: Replace by some lexer generator
--   Parser combinator is a powerful tool.
--   However, it has overhead to build parser at runtime.
--
module Language.Quell.Parse.Lexer where

import Language.Quell.Prelude
import Language.Quell.Parse.Type


lexer :: MonadIO m => (Spanned Token -> ParseCtx m a) -> ParseCtx m a
lexer = undefined
