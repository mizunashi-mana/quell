module Language.Quell.Type.Token (

) where

import Language.Quell.Prelude


data Token
  = TokKwAs
  | TokKwAssocLeft
  | TokKwAssocRight
  | TokKwCase
  | TokKwClass
  | TokKwData
  | TokKwDefault
  | TokKwDeriving
  | TokKwDo
  | TokKwElse
  | TokKwForall
  | TokKwHiding
  | TokKwIf
  | TokKwImport
  | TokKwIn
  | TokKwInfix
  | TokKwInstance
  | TokKwLet
  | TokKwModule
  | TokKwNewtype
  | TokKwNone
  | TokKwOf
  | TokKwSignature
  | TokKwThen
  | TokKwType
  | TokKwWhere

  | TokSymArrow
  | TokSymAt
  | TokSymBang
  | TokSymColon
  | TokSymDArrow
  | TokSymDot
  | TokSymEqual
  | TokSymLamPref
  | TokSymQuestion
  | TokSymUnderscore

  | TokSpOpenCurly
  | TokSpCloseCurly
  | TokSpOpenVCurly
  | TokSpCloseVCurly
  | TokSpOpenBrack
  | TokSpCloseBrack
  | TokSpOpenParen
  | TokSpCloseParen
  | TokSpBackquote
  | TokSpComma
  | TokSpSemi

  | TokIdConId Text
  | TokIdConSym Text
  | TokIdVarId Text
  | TokIdVarSym Text
  | TokIdQualConId Text Text
  | TokIdQualConSym Text Text
  | TokIdQualVarId Text Text
  | TokIdQualVarSym Text Text

  | TokLitChar Char
  | TokLitString Text
  | TokLitInteger Integer
  | TokLitRational Rational

  | TokCommentBlock Text
  | TokCommentLine Text

  | TokEndOfFile
  deriving (Eq, Show)

instance Pretty Token where
  pretty = \case
    TokKwAs               -> pretty "as"
    TokKwAssocLeft        -> pretty "assocL"
    TokKwAssocRight       -> pretty "assocR"
    TokKwCase             -> pretty "case"
    TokKwClass            -> pretty "class"
    TokKwData             -> pretty "data"
    TokKwDefault          -> pretty "default"
    TokKwDeriving         -> pretty "deriving"
    TokKwDo               -> pretty "do"
    TokKwElse             -> pretty "else"
    TokKwForall           -> pretty "forall"
    TokKwHiding           -> pretty "hiding"
    TokKwIf               -> pretty "if"
    TokKwImport           -> pretty "import"
    TokKwIn               -> pretty "in"
    TokKwInfix            -> pretty "infix"
    TokKwInstance         -> pretty "instance"
    TokKwLet              -> pretty "let"
    TokKwModule           -> pretty "module"
    TokKwNewtype          -> pretty "newtype"
    TokKwNone             -> pretty "none"
    TokKwOf               -> pretty "of"
    TokKwSignature        -> pretty "sig"
    TokKwThen             -> pretty "then"
    TokKwType             -> pretty "type"
    TokKwWhere            -> pretty "where"
    TokSymArrow           -> pretty "->"
    TokSymAt              -> pretty "@"
    TokSymBang            -> pretty "!"
    TokSymColon           -> pretty ":"
    TokSymDArrow          -> pretty "=>"
    TokSymDot             -> pretty "."
    TokSymEqual           -> pretty "="
    TokSymLamPref         -> pretty "\\"
    TokSymQuestion        -> pretty "?"
    TokSymUnderscore      -> pretty "_"
    TokSpOpenCurly        -> pretty "("
    TokSpCloseCurly       -> pretty ")"
    TokSpOpenBrack        -> pretty "{"
    TokSpCloseBrack       -> pretty "}"
    TokSpOpenVCurly       -> pretty "{"
    TokSpCloseVCurly      -> pretty "}"
    TokSpOpenParen        -> pretty "("
    TokSpCloseParen       -> pretty ")"
    TokSpBackquote        -> pretty "`"
    TokSpComma            -> pretty ","
    TokSpSemi             -> pretty ";"
    TokIdConId v          -> pretty v
    TokIdConSym v         -> pretty v
    TokIdVarId v          -> pretty v
    TokIdVarSym v         -> pretty v
    TokIdQualConId qv v   -> pretty do qv <> "." <> v
    TokIdQualConSym qv v  -> pretty do qv <> "." <> v
    TokIdQualVarId qv v   -> pretty do qv <> "." <> v
    TokIdQualVarSym qv v  -> pretty do qv <> "." <> v
    TokLitChar v          -> pretty do show v
    TokLitString v        -> pretty do show v
    TokLitInteger v       -> pretty v
    TokLitRational v      -> pretty v
    TokCommentBlock v     -> pretty do "{-" <> v <> "-}"
    TokCommentLine v      -> pretty do "--" <> v
    TokEndOfSource        -> mempty
