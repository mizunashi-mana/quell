{
module Language.Quell.Parsing.Parser (

) where

import Language.Quell.Prelude

import qualified Language.Quell.Type.Token
}

%token
  'as'       { S TokKwAs }
  'case'     { S TokKwCase }
  'class'    { S TokKwClass }
  'data'     { S TokKwData }
  'default'  { S TokKwDefault }
  'deriving' { S TokKwDeriving }
  'do'       { S TokKwDo }
  'else'     { S TokKwElse }
  'forall'   { S TokKwForall }
  'hiding'   { S TokKwHiding }
  'if'       { S TokKwIf }
  'import'   { S TokKwImport }
  'in'       { S TokKwIn }
  'infix'    { S TokKwInfix }
  'infixl'   { S TokKwInfixl }
  'infixr'   { S TokKwInfixr }
  'instance' { S TokKwInstance }
  'let'      { S TokKwLet }
  'module'   { S TokKwModule }
  'newtype'  { S TokKwNewtype }
  'of'       { S TokKwOf }
  'sig'      { S TokKwSignature }
  'then'     { S TokKwThen }
  'type'     { S TokKwType }
  'where'    { S TokKwWhere }

  '->'       { S TokSymKwArrow }
  '@'        { S TokSymKwAt }
  ':'        { S TokSymKwColon }
  '.'        { S TokSymKwDot }
  '='        { S TokSymKwEqual }
  '\\'       { S TokSymKwLamPref }
  '?'        { S TokSymKwQuestion }

  '{'        { S TokSpOpenCurly }
  '}'        { S TokSpCloseCurly }
  VOCURLY    { S TokSpOpenVCurly }
  VCCURLY    { S TokSpCloseVCurly }
  '['        { S TokSpOpenBrack }
  ']'        { S TokSpCloseBrack }
  '('        { S TokSpOpenParen }
  ')'        { S TokSpCloseParen }
  '`'        { S TokSpBackquote }
  ','        { S TokSpComma }
  ';'        { S TokSpSemi }
  '_'        { S TokSpUnderscore }

  CONID      { S (TokIdConId _) }
  CONSYM     { S (TokIdConSym _) }
  VARID      { S (TokIdVarId _) }
  VARSYM     { S (TokIdVarSym _) }
  QCONID     { S (TokIdQualConId _) }
  QCONSYM    { S (TokIdQualConSym _) }
  QVARID     { S (TokIdQualVarId _) }
  QVARSYM    { S (TokIdQualVarSym _) }

  CHAR       { S (TokLitChar _) }
  STRING     { S (TokLitString _) }
  INTEGER    { S (TokLitInteger _) }
  RATIONAL   { S (TokLitRational _) }

%monad { P }{ >>= }{ return }
{
pattern S t = Spanned _ t
}
