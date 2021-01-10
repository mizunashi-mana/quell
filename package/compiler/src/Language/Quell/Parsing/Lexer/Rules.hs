{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TemplateHaskell #-}

module Language.Quell.Parsing.Lexer.Rules where

import Language.Quell.Prelude

import qualified Data.CharSet                        as CharSet
import qualified Data.CharSet.Unicode                as UniCharSet
import qualified Data.Word                           as Word
import qualified Language.Haskell.TH                 as TH
import qualified Language.Lexer.Tlex                 as Tlex
import qualified Language.Lexer.Tlex.Plugin.Encoding as TlexEnc
import qualified Language.Lexer.Tlex.Plugin.TH       as TlexTH
import qualified Language.Quell.Parsing.Token        as Token


data LexerState = Initial
  deriving (Eq, Ord, Show, Enum)

type LexerAction = ByteString -> Token
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

  -- be before var_op to avoid conflicting
  literalRules

whiteSpaceRules :: ScannerBuilder ()
whiteSpaceRules = do
  initialRule (Tlex.someP whiteCharP) [||pure Token.WhiteSpace||]
  commentRules

literalRules :: ScannerBuilder ()
literalRules = do
  --- lex rests without standard lexer
  initialRule integerOrRationalOpenP [||undefined||]
  initialRule byteStringOpenP [||pure do Token.LitByteString undefined||]
  initialRule stringOpenP [||pure do Token.LitString undefined||]
  initialRule byteCharOpenP [||pure do Token.LitByteChar undefined||]
  initialRule charOpenP [||pure do Token.LitChar undefined||]

commentRules :: ScannerBuilder ()
commentRules = do
  initialRule lineCommentWithoutContentP [||pure do Token.CommentLine do text ""||]
  --- lex rests without standard lexer
  initialRule lineCommentOpenWithContentP [||pure do Token.CommentLine undefined||]

  initialRule multilineCommentWithoutContentP [||pure do Token.CommentMultiline do text ""]
  --- lex rests without standard lexer
  initialRule multilineCommentOpenWithContentP [||pure do Token.CommentMultiline undefined||]

  --- lex rests without standard lexer
  initialRule docCommentOpenP [||pure do Token.CommentDoc undefined||]

  --- lex rests without standard lexer
  initialRule pragmaCommentOpenP [||pure do Token.CommentPragma undefined||]


integerOrRationalOpenP = Tlex.maybeP signP <> digitP

signP = charSetP signCs
signCs = CharSet.fromList ['+', '-']


byteStringOpenP = chP '#' <> strSepP

stringOpenP = strSepP

byteCharOpenP = chP '#' <> charSepP

charOpenP = charSepP

strSepP = chP '"'

charSepP = chP '\''


lineCommentWithoutContentP = lineCommentOpenP <> newlineP
lineCommentOpenWithContentP = lineCommentOpenP
  <> charSetP do anyCs `CharSet.difference` mconcat
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

otherCs = mconcat
  [
    CharSet.fromList ['\''],
    UniCharSet.modifierLetter,
    UniCharSet.mark,
    UniCharSet.letterNumber,
    UniCharSet.format `CharSet.difference` whiteCharCs
  ]

otherSpecialCs =
  CharSet.fromList [';', '#', '"', '{', '}']

otherGraphicCs = mconcat
  [
    UniCharSet.punctuation `CharSet.difference` symbolCharCs
  ]


charSetP :: CharSet.CharSet -> Pattern
charSetP cs = TlexEnc.charSetP TlexEnc.charSetPUtf8 cs

chP :: Char -> Pattern
chP c = TlexEnc.chP TlexEnc.charSetPUtf8 c

stringP :: String -> Pattern
stringP s = TlexEnc.stringP TlexEnc.charSetPUtf8 s
