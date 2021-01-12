{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TemplateHaskell #-}

module Language.Quell.Parsing.Lexer.Rules where

import           Language.Quell.Prelude

import qualified Language.Haskell.TH                 as TH
import qualified Language.Lexer.Tlex                 as Tlex
import qualified Language.Lexer.Tlex.Plugin.TH       as TlexTH
import qualified Language.Quell.Data.TextId          as TextId
import qualified Language.Quell.Type.Token           as Token
import qualified Language.Quell.Parsing.Lexer.CodeUnit       as CodeUnit
import qualified Language.Lexer.Tlex.Data.EnumSet    as EnumSet
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

type LexerCodeUnit = CodeUnit.T
type CharSet = EnumSet.EnumSet LexerCodeUnit

instance Semigroup CharSet where
    (<>) = EnumSet.union

instance Monoid CharSet where
    mempty = EnumSet.empty

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
            do symbolCs `csDifference` charsCs [':']

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

specialCs = charsCs ['(', ')', ',', '[', ']', '`']

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
signCs = charsCs ['+', '-']


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
    do anyCs `csDifference` mconcat
        [
            symbolCs,
            otherCs
        ]
lineCommentOpenP = stringP "--" <> Tlex.manyP do chP '-'

multilineCommentWithoutContentP = commentOpenP <> commentCloseP
multilineCommentOpenWithContentP = commentOpenP
    <> charSetP do largeAnyCs `csDifference` charsCs ['!', '#']

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
        charsCs ['\v'],
        spaceCs,
        newlineCs
    ]

spaceCs = mconcat
    [
        charsCs ['\t', '\x200E', '\x200F'],
        CodeUnit.catSpaceSeparator
    ]

newlineP = Tlex.orP
    [
        stringP "\r\n",
        charSetP newlineCs
    ]
newlineCs = mconcat
    [
        charsCs ['\r', '\n', '\f'],
        CodeUnit.catLineSeparator,
        CodeUnit.catParagraphSeparator
    ]

smallP = charSetP smallCs
smallCs = mconcat
    [
        charsCs ['_'],
        CodeUnit.catLowercaseLetter,
        CodeUnit.catOtherLetter
    ]

largeP = charSetP largeCs
largeCs = mconcat
    [
        CodeUnit.catUppercaseLetter,
        CodeUnit.catTitlecaseLetter
    ]

symbolP = charSetP symbolCs
symbolCs = symbolCharCs `csDifference` mconcat
    [
        charsCs ['_', '\''],
        specialCs,
        otherSpecialCs
    ]
symbolCharCs = mconcat
    [
        CodeUnit.catConnectorPunctuation,
        CodeUnit.catDashPunctuation,
        CodeUnit.catOtherPunctuation,
        CodeUnit.catSymbol
    ]

digitP = charSetP digitCs
digitCs = mconcat
    [
        CodeUnit.catDecimalNumber,
        CodeUnit.catOtherNumber
    ]

otherP = charSetP otherCs
otherCs = mconcat
    [
        charsCs ['\''],
        CodeUnit.catModifierLetter,
        CodeUnit.catMark,
        CodeUnit.catLetterNumber,
        CodeUnit.catFormat `csDifference` whiteCharCs
    ]

otherSpecialCs = charsCs
    [';', '#', '"', '{', '}', '⦃', '⦄']

otherGraphicCs = otherGraphicCharCs `csDifference` mconcat
    [
        symbolCharCs,
        specialCs,
        otherSpecialCs
    ]
otherGraphicCharCs = mconcat
    [
        CodeUnit.catPunctuation
    ]


charsCs :: [Char] -> CharSet
charsCs cs = EnumSet.fromList
    [ e
    | c <- cs
    , let e = case CodeUnit.fromCharPoint c of
            Just x  -> x
            Nothing -> error do "Unsupported char: " <> show c
    ]

csDifference :: CharSet -> CharSet -> CharSet
csDifference = EnumSet.difference

charSetP :: CharSet -> Pattern
charSetP = Tlex.straightEnumSetP

chP :: Char -> Pattern
chP c = charSetP do charsCs [c]

stringP :: Prelude.String -> Pattern
stringP s = foldMap chP s
