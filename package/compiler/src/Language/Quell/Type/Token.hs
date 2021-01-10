module Language.Quell.Type.Token (
  T,
  Token (..),
) where

import Language.Quell.Prelude

import qualified GHC.Show as GHC
import qualified Prelude
import qualified Language.Quell.Data.TextId as TextId


type T = Token

data Token
  = KwAlias
  | KwAs
  | KwCase
  | KwData
  | KwDefault
  | KwDerive
  | KwDo
  | KwExport
  | KwFamily
  | KwForeign
  | KwImpl
  | KwIn
  | KwInfix
  | KwLet
  | KwLetrec
  | KwModule
  | KwNewtype
  | KwNone
  | KwOf
  | KwPattern
  | KwRec
  | KwRecord
  | KwRole
  | KwSelf
  | KwSignature
  | KwStatic
  | KwTrait
  | KwType
  | KwUnderscore
  | KwUse
  | KwWhen
  | KwWhere

  | LKwDefault
  | LKwSelf

  | SymArrow
  | SymAt
  | SymBang
  | SymColon
  | SymDArrow
  | SymDColon
  | SymDLeftArrow
  | SymDot
  | SymDots
  | SymEqual
  | SymForall
  | SymLambda
  | SymLeftArrow
  | SymOr
  | SymTilde
  | SymUnknown

  | SpBackquote
  | SpBrackOpen
  | SpBrackClose
  | SpComma
  | SpCurlyOpen
  | SpCurlyClose
  | SpDCurlyOpen
  | SpDCurlyClose
  | SpParenOpen
  | SpParenClose
  | SpSemis
  | SpVCurlyOpen
  | SpVCurlyClose
  | SpVDCurlyOpen
  | SpVDCurlyClose
  | SpVSemis

  | IdConId TextId.T
  | IdConSym TextId.T
  | IdVarId TextId.T
  | IdVarSym TextId.T

  | LitByteChar Word8
  | LitByteString ByteString
  | LitInteger Integer
  | LitRational Rational
  | LitChar Char
  | LitString Text

  | CommentLine Text
  | CommentBlock Text
  | CommentPragma Text
  | CommentDocBlock Text

  | Whitespace
  | EndOfSource
  deriving (Eq, Show)

instance Pretty Token where
  pretty = \case
    KwAlias             -> pretty "alias"
    KwAs                -> pretty "as"
    KwCase              -> pretty "case"
    KwData              -> pretty "data"
    KwDefault           -> pretty "default"
    KwDerive            -> pretty "derive"
    KwDo                -> pretty "do"
    KwExport            -> pretty "export"
    KwFamily            -> pretty "family"
    KwForeign           -> pretty "foreign"
    KwImpl              -> pretty "impl"
    KwIn                -> pretty "in"
    KwInfix             -> pretty "infix"
    KwLet               -> pretty "let"
    KwLetrec            -> pretty "letrec"
    KwModule            -> pretty "module"
    KwNewtype           -> pretty "newtype"
    KwNone              -> pretty "none"
    KwOf                -> pretty "of"
    KwPattern           -> pretty "pattern"
    KwRec               -> pretty "rec"
    KwRecord            -> pretty "record"
    KwRole              -> pretty "role"
    KwSelf              -> pretty "self"
    KwSignature         -> pretty "signature"
    KwStatic            -> pretty "static"
    KwTrait             -> pretty "trait"
    KwType              -> pretty "type"
    KwUnderscore        -> pretty "_"
    KwUse               -> pretty "use"
    KwWhen              -> pretty "when"
    KwWhere             -> pretty "where"
    LKwDefault          -> pretty "Default"
    LKwSelf             -> pretty "Self"
    SymArrow            -> pretty "->"
    SymAt               -> pretty "@"
    SymBang             -> pretty "!"
    SymColon            -> pretty ":"
    SymDArrow           -> pretty "=>"
    SymDColon           -> pretty "::"
    SymDLeftArrow       -> pretty "<="
    SymDot              -> pretty "."
    SymDots             -> pretty ".."
    SymEqual            -> pretty "="
    SymForall           -> pretty "\\/"
    SymLambda           -> pretty "\\"
    SymLeftArrow        -> pretty "<-"
    SymOr               -> pretty "|"
    SymTilde            -> pretty "~"
    SymUnknown          -> pretty "?"
    SpBackquote         -> pretty "`"
    SpBrackOpen         -> pretty "["
    SpBrackClose        -> pretty "]"
    SpComma             -> pretty ","
    SpCurlyOpen         -> pretty "{"
    SpCurlyClose        -> pretty "}"
    SpDCurlyOpen        -> pretty "{{"
    SpDCurlyClose       -> pretty "}}"
    SpParenOpen         -> pretty "("
    SpParenClose        -> pretty ")"
    SpSemis             -> pretty ";"
    SpVCurlyOpen        -> pretty "{- { -}"
    SpVCurlyClose       -> pretty "{- } -}"
    SpVDCurlyOpen       -> pretty "{- {{ -}"
    SpVDCurlyClose      -> pretty "{- }} -}"
    SpVSemis            -> pretty "{- ; -}"
    IdConId v           -> pretty v
    IdConSym v          -> pretty v
    IdVarId v           -> pretty v
    IdVarSym v          -> pretty v
    LitByteChar v       -> prettyByteChar v
    LitByteString v     -> prettyByteString v
    LitChar v           -> pretty do show v
    LitString v         -> pretty do show v
    LitInteger v        -> pretty v
    LitRational v       -> prettyRational v
    CommentLine v       -> pretty do text "--" <> v
    CommentBlock v      -> pretty do text "{-" <> v <> text "-}"
    CommentPragma v     -> pretty do text "{-#" <> v <> text "#-}"
    CommentDocBlock v   -> pretty do text "{-!" <> v <> text "\n|-}"
    Whitespace          -> mempty
    EndOfSource         -> mempty

-- FIXME: Current implementation is approximately.
-- Make to show complete representation.
prettyRational :: Rational -> Doc ann
prettyRational v = pretty do fromRational @Double v

prettyByteChar :: Word8 -> Doc ann
prettyByteChar w = if
  | w <= 0x7F ->
    let c = toEnum @Char do fromInteger @Int do toInteger w
    in pretty do "#" <> show c
  | otherwise -> pretty do "#'\\x" <> showByteHex w <> "'"

prettyByteString :: ByteString -> Doc ann
prettyByteString bs = pretty do "#\"" <> bsShow <> "\"" where
  bsShow = ofoldMap
    do \w -> if
        | w <= 0x7F ->
          let c = toEnum do fromIntegral w
          in GHC.showLitChar c ""
        | otherwise -> "\\x" <> showByteHex w
    bs

showByteHex :: Word8 -> Prelude.String
showByteHex = \w -> if
  | w <= 0xF  -> "0" <> showByteHex1Digit w
  | otherwise -> showByteHex1Digit (w `div` 0x10) <> showByteHex1Digit (w `mod` 0x10)
  where
    showByteHex1Digit w = if
      | w <= 0x9 -> show w
      | otherwise -> [toEnum do fromEnum 'A' - 0xA + fromIntegral w]
