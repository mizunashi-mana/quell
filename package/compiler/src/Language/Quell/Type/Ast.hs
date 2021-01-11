module Language.Quell.Type.Ast (
  Program (..),
  Decl (..),
) where

import           Language.Quell.Prelude


data Program = Program {
  moduleName :: Text,
  decls      :: [Decl]
} deriving (Eq, Show)

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
