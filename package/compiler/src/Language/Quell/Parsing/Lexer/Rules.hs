{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TemplateHaskell #-}

module Language.Quell.Parsing.Lexer.Rules where

import           Language.Quell.Prelude

import qualified Data.CharSet                        as CharSet
import qualified Data.CharSet.Unicode                as UniCharSet
import qualified Data.Word                           as Word
import qualified Language.Haskell.TH                 as TH
import qualified Language.Lexer.Tlex                 as Tlex
import qualified Language.Lexer.Tlex.Plugin.Encoding as TlexEnc
import qualified Language.Lexer.Tlex.Plugin.TH       as TlexTH
import qualified Language.Quell.Data.TextId          as TextId
import qualified Language.Quell.Type.Token           as Token
import qualified Prelude


data LexerState = Initial
  deriving (Eq, Ord, Show, Enum)

data LexerAction
  = WithToken Token.T
  | WithIdToken (TextId.T -> Token.T)
  | LexWhitespace
  | LexLitIntegerOrRational
  | LexLitByteString
  | LexLitByteChar
  | LexLitString
  | LexLitChar
  | LexCommentLineWithContent
  | LexCommentMultilineWithContent
  | LexCommentDoc
  | LexCommentPragma

type LexerCodeUnit = Word.Word8


type ScannerBuilder = TlexTH.THScannerBuilder LexerState LexerCodeUnit LexerAction
type Pattern = Tlex.Pattern LexerCodeUnit

initialRule :: Pattern -> TH.Q (TH.TExp LexerAction) -> ScannerBuilder ()
initialRule = TlexTH.thLexRule [Initial]

buildLexer :: TH.Q [TH.Dec]
buildLexer = do
    stateTy <- [t|LexerState|]
    codeUnitTy <- [t|LexerCodeUnit|]
    actionTy <- [t|LexerAction|]
    let lexer = TlexTH.buildTHScanner codeUnitTy stateTy actionTy lexerRules
    TlexTH.outputScanner lexer

lexerRules :: ScannerBuilder ()
lexerRules = do
  whiteSpaceRules

  -- be before varOpRule to avoid conflicting
  literalRules

  specialRules
  semisRules
  braceRules

  -- be before varIdRule / conIdRule to avoid conflicting
  reservedIdRules
  -- be before varOpRule / conOpRule to avoid conflicting
  reservedOpRules


varIdRule :: ScannerBuilder ()
varIdRule =
  initialRule varIdP [||WithIdToken Token.IdVarId||]

varIdP = smallP <> Tlex.manyP do
  Tlex.orP
    [
      smallP,
      largeP,
      digitP,
      otherP
    ]

conIdRule :: ScannerBuilder ()
conIdRule =
  initialRule conIdP [||WithIdToken Token.IdConId||]

conIdP = largeP <> Tlex.manyP do
  Tlex.orP
    [
      smallP,
      largeP,
      digitP,
      otherP
    ]

varOpRule :: ScannerBuilder ()
varOpRule =
  initialRule varOpP [||WithIdToken Token.IdVarOp||]

varOpP = symbolWithoutColonP <> Tlex.manyP do
  Tlex.orP
    [
      symbolP,
      otherP
    ]
  where
    symbolWithoutColonP = charSetP
      do symbolCs `CharSet.difference` CharSet.fromList [':']

conOpRule :: ScannerBuilder ()
conOpRule =
  initialRule conOpP [||WithIdToken Token.IdConOp||]

conOpP = chP ':' <> Tlex.manyP do
  Tlex.orP
    [
      symbolP,
      otherP
    ]


reservedIdRules :: ScannerBuilder ()
reservedIdRules = do
  initialRule (stringP "alias")     [||WithToken do Token.KwAlias||]
  initialRule (stringP "as")        [||WithToken do Token.KwAs||]
  initialRule (stringP "case")      [||WithToken do Token.KwCase||]
  initialRule (stringP "data")      [||WithToken do Token.KwData||]
  initialRule (stringP "derive")    [||WithToken do Token.KwDerive||]
  initialRule (stringP "do")        [||WithToken do Token.KwDo||]
  initialRule (stringP "export")    [||WithToken do Token.KwExport||]
  initialRule (stringP "family")    [||WithToken do Token.KwFamily||]
  initialRule (stringP "foreign")   [||WithToken do Token.KwForeign||]
  initialRule (stringP "impl")      [||WithToken do Token.KwImpl||]
  initialRule (stringP "infix")     [||WithToken do Token.KwInfix||]
  initialRule (stringP "in")        [||WithToken do Token.KwIn||]
  initialRule (stringP "letrec")    [||WithToken do Token.KwLetrec||]
  initialRule (stringP "let")       [||WithToken do Token.KwLet||]
  initialRule (stringP "module")    [||WithToken do Token.KwModule||]
  initialRule (stringP "newtype")   [||WithToken do Token.KwNewtype||]
  initialRule (stringP "none")      [||WithToken do Token.KwNone||]
  initialRule (stringP "of")        [||WithToken do Token.KwOf||]
  initialRule (stringP "pattern")   [||WithToken do Token.KwPattern||]
  initialRule (stringP "record")    [||WithToken do Token.KwRecord||]
  initialRule (stringP "rec")       [||WithToken do Token.KwRec||]
  initialRule (stringP "role")      [||WithToken do Token.KwRole||]
  initialRule (stringP "signature") [||WithToken do Token.KwSignature||]
  initialRule (stringP "static")    [||WithToken do Token.KwStatic||]
  initialRule (stringP "trait")     [||WithToken do Token.KwTrait||]
  initialRule (stringP "type")      [||WithToken do Token.KwType||]
  initialRule (stringP "use")       [||WithToken do Token.KwUse||]
  initialRule (stringP "when")      [||WithToken do Token.KwWhen||]
  initialRule (stringP "where")     [||WithToken do Token.KwWhere||]
  initialRule (stringP "_")         [||WithToken do Token.KwUnderscore||]

  initialRule (stringP "Default")   [||WithToken do Token.LKwDefault||]
  initialRule (stringP "Self")      [||WithToken do Token.LKwSelf||]

reservedOpRules :: ScannerBuilder ()
reservedOpRules = do
  initialRule (stringP "!")       [||WithToken do Token.SymBang||]
  initialRule (stringP "->")      [||WithToken do Token.SymArrow||]
  initialRule (stringP "..")      [||WithToken do Token.SymDots||]
  initialRule (stringP ".")       [||WithToken do Token.SymDot||]
  initialRule (stringP "<-")      [||WithToken do Token.SymLeftArrow||]
  initialRule (stringP "<=")      [||WithToken do Token.SymDLeftArrow||]
  initialRule (stringP "=>")      [||WithToken do Token.SymDArrow||]
  initialRule (stringP "=")       [||WithToken do Token.SymEqual||]
  initialRule (stringP "?")       [||WithToken do Token.SymUnknown||]
  initialRule (stringP "@")       [||WithToken do Token.SymAt||]
  initialRule (stringP "\\/")     [||WithToken do Token.SymForall||]
  initialRule (stringP "\\")      [||WithToken do Token.SymLambda||]
  initialRule (stringP "|")       [||WithToken do Token.SymOr||]
  initialRule (stringP "~")       [||WithToken do Token.SymTilde||]
  initialRule (stringP "∀")       [||WithToken do Token.SymForall||]
  initialRule (stringP "λ")       [||WithToken do Token.SymLambda||]
  initialRule (stringP "→")       [||WithToken do Token.SymArrow||]
  initialRule (stringP "←")       [||WithToken do Token.SymLeftArrow||]
  initialRule (stringP "⇒")       [||WithToken do Token.SymDArrow||]
  initialRule (stringP "⇐")       [||WithToken do Token.SymDLeftArrow||]
  initialRule (stringP "…")       [||WithToken do Token.SymDots||]

specialRules :: ScannerBuilder ()
specialRules = do
  initialRule (stringP "(")       [||WithToken do Token.SpParenOpen||]
  initialRule (stringP ")")       [||WithToken do Token.SpParenClose||]
  initialRule (stringP ",")       [||WithToken do Token.SpComma||]
  initialRule (stringP "[")       [||WithToken do Token.SpBrackOpen||]
  initialRule (stringP "]")       [||WithToken do Token.SpBrackClose||]
  initialRule (stringP "`")       [||WithToken do Token.SpBackquote||]

specialCs = CharSet.fromList ['(', ')', ',', '[', ']', '`']

braceRules :: ScannerBuilder ()
braceRules = do
  initialRule (stringP "{{")        [||WithToken do Token.SpDBraceOpen||]
  initialRule (stringP "}}")        [||WithToken do Token.SpDBraceClose||]
  initialRule (stringP "{")         [||WithToken do Token.SpBraceOpen||]
  initialRule (stringP "}")         [||WithToken do Token.SpBraceClose||]
  initialRule (stringP "⦃")         [||WithToken do Token.SpDBraceOpen||]
  initialRule (stringP "⦄")         [||WithToken do Token.SpDBraceClose||]

semisRules :: ScannerBuilder ()
semisRules =
  initialRule (Tlex.someP do chP ';') [||WithToken do Token.SpSemis||]


literalRules :: ScannerBuilder ()
literalRules = do
  -- lex rests without standard lexer
  initialRule integerOrRationalOpenP [||LexLitIntegerOrRational||]
  initialRule byteStringOpenP [||LexLitByteString||]
  initialRule stringOpenP [||LexLitString||]
  initialRule byteCharOpenP [||LexLitByteChar||]
  initialRule charOpenP [||LexLitChar||]


integerOrRationalOpenP = Tlex.maybeP signP <> digitP

signP = charSetP signCs
signCs = CharSet.fromList ['+', '-']


byteStringOpenP = chP '#' <> strSepP

stringOpenP = strSepP

byteCharOpenP = chP '#' <> charSepP

charOpenP = charSepP

strSepP = chP '"'

charSepP = chP '\''


whiteSpaceRules :: ScannerBuilder ()
whiteSpaceRules = do
  -- lex rests without standard lexer
  initialRule whiteCharP [||LexWhitespace||]

  commentRules


commentRules :: ScannerBuilder ()
commentRules = do
  initialRule lineCommentWithoutContentP [||WithToken do Token.CommentLine do text ""||]
  --- lex rests without standard lexer
  initialRule lineCommentOpenWithContentP [||LexCommentLineWithContent||]

  initialRule multilineCommentWithoutContentP [||WithToken do Token.CommentMultiline do text ""||]
  --- lex rests without standard lexer
  initialRule multilineCommentOpenWithContentP [||LexCommentMultilineWithContent||]

  --- lex rests without standard lexer
  initialRule docCommentOpenP [||LexCommentDoc||]

  --- lex rests without standard lexer
  initialRule pragmaCommentOpenP [||LexCommentPragma||]

lineCommentWithoutContentP = lineCommentOpenP <> newlineP
lineCommentOpenWithContentP = lineCommentOpenP <> charSetP
  do anyCs `CharSet.difference` mconcat
      [
        symbolCs,
        otherCs
      ]
lineCommentOpenP = stringP "--" <> Tlex.manyP do chP '-'

multilineCommentWithoutContentP = commentOpenP <> commentCloseP
multilineCommentOpenWithContentP = commentOpenP
  <> charSetP do largeAnyCs `CharSet.difference` CharSet.fromList ['!', '#']

docCommentOpenP = commentOpenP <> chP '!'

pragmaCommentOpenP = commentOpenP <> chP '#'

commentOpenP = stringP "{-"
commentCloseP = stringP "-}"

anyCs = mconcat
  [
    graphicCs,
    spaceCs
  ]

largeAnyCs = mconcat
  [
    graphicCs,
    whiteCharCs
  ]


graphicCs = mconcat
  [
    smallCs,
    largeCs,
    symbolCs,
    digitCs,
    otherCs,
    specialCs,
    otherSpecialCs,
    otherGraphicCs
  ]

whiteCharP = charSetP whiteCharCs
whiteCharCs = mconcat
  [
    CharSet.fromList ['\v'],
    spaceCs,
    newlineCs
  ]

spaceCs = mconcat
  [
    CharSet.fromList ['\t', '\x200E', '\x200F'],
    UniCharSet.space
  ]

newlineP = Tlex.orP
  [
    stringP "\r\n",
    charSetP newlineCs
  ]
newlineCs = mconcat
  [
    CharSet.fromList ['\r', '\n', '\f'],
    UniCharSet.lineSeparator,
    UniCharSet.paragraphSeparator
  ]

smallP = charSetP smallCs
smallCs = mconcat
  [
    CharSet.fromList ['_'],
    UniCharSet.lowercaseLetter,
    UniCharSet.otherLetter
  ]

largeP = charSetP largeCs
largeCs = mconcat
  [
    UniCharSet.uppercaseLetter,
    UniCharSet.titlecaseLetter
  ]

symbolP = charSetP symbolCs
symbolCs = symbolCharCs `CharSet.difference` mconcat
  [
    CharSet.fromList ['_', '\''],
    specialCs,
    otherSpecialCs
  ]
symbolCharCs = mconcat
  [
    UniCharSet.connectorPunctuation,
    UniCharSet.dashPunctuation,
    UniCharSet.otherPunctuation,
    UniCharSet.symbol
  ]

digitP = charSetP digitCs
digitCs = mconcat
  [
    UniCharSet.decimalNumber,
    UniCharSet.otherNumber
  ]

otherP = charSetP otherCs
otherCs = mconcat
  [
    CharSet.fromList ['\''],
    UniCharSet.modifierLetter,
    UniCharSet.mark,
    UniCharSet.letterNumber,
    UniCharSet.format `CharSet.difference` whiteCharCs
  ]

otherSpecialCs =
  CharSet.fromList [';', '#', '"', '{', '}', '⦃', '⦄']

otherGraphicCs = otherGraphicCharCs `CharSet.difference` mconcat
  [
    symbolCharCs,
    specialCs,
    otherSpecialCs
  ]
otherGraphicCharCs = mconcat
  [
    UniCharSet.punctuation
  ]


charSetP :: CharSet.CharSet -> Pattern
charSetP cs = TlexEnc.charSetP TlexEnc.charSetPUtf8 cs

chP :: Char -> Pattern
chP c = TlexEnc.chP TlexEnc.charSetPUtf8 c

stringP :: Prelude.String -> Pattern
stringP s = TlexEnc.stringP TlexEnc.charSetPUtf8 s
