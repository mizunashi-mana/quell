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
    initialRule (Tlex.someP whiteCharP) [||\_ -> TokWhiteSpace||]

    initialRule commentP [||TokLineComment||]

    -- openComP should be the head on nested comment mode to avoid conflicting.
    TlexTH.thLexRule [Initial, NestedComment] openComP [||\_ -> TokOpenComment||]
    -- closeComP should be the head on nested comment mode to avoid conflicting.
    nestedCommentRule closeComP [||\_ -> TokCloseComment||]
    nestedCommentRule anyWithNewlineP [||TokEnclosedCommentChar||]

    initialRule specialP [||TokSpecial||]

    -- reservedIdP should be before qvarid to avoid conflicting.
    initialRule reservedIdP [||TokReservedId||]
    -- reservedOpP should be before qvarsym / qconsym to avoid conflicting.
    initialRule reservedOpP [||TokReservedOp||]

    initialRule qvaridP [||TokQualifiedVarId||]
    initialRule qconidP [||TokQualifiedConId||]
    initialRule qvarsymP [||TokQualifiedVarSym||]
    initialRule qconsymP [||TokQualifiedConSym||]

    initialRule litIntegerP [||TokLitInteger||]
    initialRule litFloatP [||TokLitFloat||]
    initialRule litCharP [||TokLitChar||]
    initialRule litStringP [||TokLitString||]


charSetP :: CharSet.CharSet -> Pattern
charSetP cs = TlexEnc.charSetP TlexEnc.charSetPUtf8 cs

chP :: Char -> Pattern
chP c = TlexEnc.chP TlexEnc.charSetPUtf8 c

charsP :: [Char] -> Pattern
charsP cs = TlexEnc.charsP TlexEnc.charSetPUtf8 cs

stringP :: String -> Pattern
stringP s = TlexEnc.stringP TlexEnc.charSetPUtf8 s
