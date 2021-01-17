module Language.Quell.Type.Token (
  T,
  Token (..),
) where

import           Language.Quell.Prelude

import qualified GHC.Show                   as GHC
import qualified Language.Quell.Data.TextId as TextId
import qualified Prelude


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
  | SpBraceOpen
  | SpBraceClose
  | SpDBraceOpen
  | SpDBraceClose
  | SpParenOpen
  | SpParenClose
  | SpSemi
  | SpVBraceOpen
  | SpVBraceClose
  | SpVDBraceOpen
  | SpVDBraceClose
  | SpVSemi

  | IdConId TextId.T
  | IdConOp TextId.T
  | IdVarId TextId.T
  | IdVarOp TextId.T

  | LitByteChar Word8
  | LitByteString ByteString
  | LitInteger Integer
  | LitRational Rational
  | LitChar Char
  | LitString Text
  | LitInterpStringWithoutInterp Text
  | LitInterpStringStart Text
  | LitInterpStringContinue Text
  | LitInterpStringEnd Text

  | CommentLine Text
  | CommentMultiline Text
  | CommentPragma Text
  | CommentDoc Text
  deriving (Eq, Show)

instance Pretty Token where
  pretty = \case
    KwAlias                         -> pretty "alias"
    KwAs                            -> pretty "as"
    KwCase                          -> pretty "case"
    KwData                          -> pretty "data"
    KwDefault                       -> pretty "default"
    KwDerive                        -> pretty "derive"
    KwDo                            -> pretty "do"
    KwExport                        -> pretty "export"
    KwFamily                        -> pretty "family"
    KwForeign                       -> pretty "foreign"
    KwImpl                          -> pretty "impl"
    KwIn                            -> pretty "in"
    KwInfix                         -> pretty "infix"
    KwLet                           -> pretty "let"
    KwLetrec                        -> pretty "letrec"
    KwModule                        -> pretty "module"
    KwNewtype                       -> pretty "newtype"
    KwNone                          -> pretty "none"
    KwOf                            -> pretty "of"
    KwPattern                       -> pretty "pattern"
    KwRec                           -> pretty "rec"
    KwRecord                        -> pretty "record"
    KwRole                          -> pretty "role"
    KwSelf                          -> pretty "self"
    KwSignature                     -> pretty "signature"
    KwStatic                        -> pretty "static"
    KwTrait                         -> pretty "trait"
    KwType                          -> pretty "type"
    KwUnderscore                    -> pretty "_"
    KwUse                           -> pretty "use"
    KwWhen                          -> pretty "when"
    KwWhere                         -> pretty "where"
    LKwDefault                      -> pretty "Default"
    LKwSelf                         -> pretty "Self"
    SymArrow                        -> pretty "->"
    SymAt                           -> pretty "@"
    SymBang                         -> pretty "!"
    SymColon                        -> pretty ":"
    SymDArrow                       -> pretty "=>"
    SymDColon                       -> pretty "::"
    SymDLeftArrow                   -> pretty "<="
    SymDot                          -> pretty "."
    SymDots                         -> pretty ".."
    SymEqual                        -> pretty "="
    SymForall                       -> pretty "\\/"
    SymLambda                       -> pretty "\\"
    SymLeftArrow                    -> pretty "<-"
    SymOr                           -> pretty "|"
    SymTilde                        -> pretty "~"
    SymUnknown                      -> pretty "?"
    SpBackquote                     -> pretty "`"
    SpBrackOpen                     -> pretty "["
    SpBrackClose                    -> pretty "]"
    SpComma                         -> pretty ","
    SpBraceOpen                     -> pretty "{"
    SpBraceClose                    -> pretty "}"
    SpDBraceOpen                    -> pretty "{{"
    SpDBraceClose                   -> pretty "}}"
    SpParenOpen                     -> pretty "("
    SpParenClose                    -> pretty ")"
    SpSemi                          -> pretty ";"
    SpVBraceOpen                    -> pretty "{- { -}"
    SpVBraceClose                   -> pretty "{- } -}"
    SpVDBraceOpen                   -> pretty "{- {{ -}"
    SpVDBraceClose                  -> pretty "{- }} -}"
    SpVSemi                         -> pretty ""
    IdConId v                       -> pretty v
    IdConOp v                       -> pretty v
    IdVarId v                       -> pretty v
    IdVarOp v                       -> pretty v
    LitByteChar v                   -> prettyByteChar v
    LitByteString v                 -> prettyByteString v
    LitChar v                       -> pretty do text "'" <> opoint v <> text "'" -- FIXME: escape
    LitString v                     -> pretty do text "\"" <> v <> text "\"" -- FIXME: escape
    LitInteger v                    -> pretty v
    LitRational v                   -> prettyRational v
    LitInterpStringWithoutInterp v  -> pretty do text "#s\"" <> v <> text "\"" -- FIXME: escape
    LitInterpStringStart v          -> pretty do text "#s\"" <> v <> text "${#" -- FIXME: escape
    LitInterpStringContinue v       -> pretty do text "#}" <> v <> text "${#" -- FIXME: escape
    LitInterpStringEnd v            -> pretty do text "#}" <> v <> text "\"" -- FIXME: escape
    CommentLine v                   -> pretty do text "--" <> v
    CommentMultiline v              -> pretty do text "{-" <> v <> text "-}"
    CommentPragma v                 -> pretty do text "{-#" <> v <> text "#-}"
    CommentDoc v                    -> pretty do text "{-!" <> v <> text "\n|-}"

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
            | w <= 0x9  -> show w
            | otherwise -> [toEnum do fromEnum 'A' - 0xA + fromIntegral w]
