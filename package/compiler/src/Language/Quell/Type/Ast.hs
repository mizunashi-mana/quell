module Language.Quell.Type.Ast (
  Decl (..),
) where


data Decl
  = DeclClass
  | DeclData
  | DeclFixity
  | DeclImport
  | DeclInstance
  | DeclModule
  | DeclNewtype
  | DeclSignature
  | DeclTypeAlias
  | DeclValue
  deriving (Eq, Show)
