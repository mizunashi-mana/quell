{

module Language.Quell.Parse.Parser (
  parse,
  parseProgram,
) where

import Language.Quell.Prelude

import Language.Quell.Parse.Lexer
import Language.Quell.Parse.Type
import Language.Quell.Type.Ast

-- for Happy
import Prelude                      (String)

}

%tokentype { Spanned Token }
%token
  'let'  { S TLet }

%monad { ParseCtx IO } { >>= } { pure }
%lexer { lexer } { S TEof }

%name parseProgram program

%%

program :: { () }
  : 'let' { () }

{

pattern S :: Token -> Spanned Token
pattern S x <- Spanned _ x

parse :: FilePath -> IO Ast
parse = undefined


-- for Happy

happyError :: ParseCtx m a
happyError = ParseCtx

}
