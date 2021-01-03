module Language.Quell.Type.Ast (
  Program (..),
  Decl (..),
) where

import Language.Quell.Prelude


data Program = Program
  { importDecls :: [ImportDecl]
  , decls :: [Decl]
  }

data ImportDecl = ImportDecl Text [Text]

data Decl
  = DeclClass
  | DeclData
  | DeclFixity
  | DeclInstance
  | DeclModule
  | DeclNewtype
  | DeclSignature
  | DeclTypeAlias
  | DeclValue
  deriving (Eq, Show)
