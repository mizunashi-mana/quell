{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TemplateHaskell #-}

module Language.Quell.Parsing.Lexer.Rules where

import           Language.Quell.Prelude

import qualified Language.Haskell.TH                   as TH
import qualified Language.Lexer.Tlex                   as Tlex
import qualified Language.Lexer.Tlex.Data.EnumSet      as EnumSet
import qualified Language.Lexer.Tlex.Plugin.TH         as TlexTH
import qualified Language.Quell.Data.TextId            as TextId
import qualified Language.Quell.Parsing.Lexer.CodeUnit as CodeUnit
import qualified Language.Quell.Type.Token             as Token


buildLexer :: TH.Q [TH.Dec]
buildLexer = do
    stateTy <- [t|LexerState|]
    codeUnitTy <- [t|LexerCodeUnit|]
    actionTy <- [t|LexerAction|]
    let lexer = TlexTH.buildTHScanner codeUnitTy stateTy actionTy lexerRules
    TlexTH.outputScanner lexer

data LexerAction
    = WithToken Token.T
    | WithIdToken IdToken
    | WithWhitespace
    | LexLitRationalWithDot
    | LexLitRationalWithoutDot
    | LexLitBitInteger
    | LexLitOctitInteger
    | LexLitHexitInteger
    | LexLitDecimalInteger
    | LexLitByteString
    | LexLitByteChar
    | LexLitString
    | LexLitChar
    | LexInterpStringStart
    | LexInterpStringContinue
    | LexCommentLineWithContent
    | LexCommentMultilineWithContent
    | LexCommentDoc
    | LexCommentPragma
    deriving (Eq, Show)

withIdToken :: (TextId.T -> Token.T) -> LexerAction
withIdToken t = WithIdToken do IdToken t

buildCharEscLexer :: TH.Q [TH.Dec]
buildCharEscLexer = do
    stateTy <- [t|()|]
    codeUnitTy <- [t|LexerCodeUnit|]
    actionTy <- [t|CharEscLexerAction|]
    let lexer = TlexTH.buildTHScanner codeUnitTy stateTy actionTy charEscRules
    TlexTH.outputScanner lexer

data CharEscLexerAction
    = WithGap
    | WithCharesc Word8
    | WithAsciiEsc Word8
    | LexUniEscape
    | LexByteesc
    deriving (Eq, Show)

data LexerState = Initial
    deriving (Eq, Ord, Show, Enum)

newtype IdToken = IdToken (TextId.T -> Token.T)

instance Eq IdToken where
    IdToken t1 == IdToken t2 =
        let t1' = t1 do TextId.stringLit "x"
            t2' = t2 do TextId.stringLit "x"
        in t1' == t2'

instance Show IdToken where
    showsPrec i (IdToken t) = showsPrec i do t do TextId.stringLit "..."
    show (IdToken t) = show do t do TextId.stringLit "..."
    showList xs = showList [ t do TextId.stringLit "..." | IdToken t <- xs ]

type LexerCodeUnit = CodeUnit.T
type CharSet = EnumSet.EnumSet LexerCodeUnit

type ScannerBuilder = TlexTH.THScannerBuilder LexerState LexerCodeUnit LexerAction
type Pattern = Tlex.Pattern LexerCodeUnit

type CharEscScannerBuilder = TlexTH.THScannerBuilder LexerState LexerCodeUnit CharEscLexerAction

initialRule :: Pattern -> TH.Q (TH.TExp LexerAction) -> ScannerBuilder ()
initialRule = TlexTH.thLexRule [Initial]

lexerRules :: ScannerBuilder ()
lexerRules = do
    whiteSpaceRules

    -- be before varOpRule to avoid conflicting
    literalRules

    specialRules
    braceRules

    -- be before varIdRule / conIdRule to avoid conflicting
    reservedIdRules
    -- be before varOpRule / conOpRule to avoid conflicting
    reservedOpRules

    varIdRule
    varOpRule
    conIdRule
    conOpRule

varIdRule :: ScannerBuilder ()
varIdRule =
    initialRule varIdP [||withIdToken Token.IdVarId||]

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
    initialRule conIdP [||withIdToken Token.IdConId||]

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
    initialRule varOpP [||withIdToken Token.IdVarOp||]

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
    initialRule conOpP [||withIdToken Token.IdConOp||]

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
    initialRule (stringP "!")           [||WithToken do Token.SymBang||]
    initialRule (stringsP ["->", "→"])  [||WithToken do Token.SymArrow||]
    initialRule (stringsP ["..", "…"])  [||WithToken do Token.SymDots||]
    initialRule (stringP ".")           [||WithToken do Token.SymDot||]
    initialRule (stringsP ["<-", "←"])  [||WithToken do Token.SymLeftArrow||]
    initialRule (stringsP ["<=", "⇐"])  [||WithToken do Token.SymDLeftArrow||]
    initialRule (stringsP ["=>", "⇒"])  [||WithToken do Token.SymDArrow||]
    initialRule (stringP "=")           [||WithToken do Token.SymEqual||]
    initialRule (stringP "?")           [||WithToken do Token.SymUnknown||]
    initialRule (stringP "@")           [||WithToken do Token.SymAt||]
    initialRule (stringsP ["\\/", "∀"]) [||WithToken do Token.SymForall||]
    initialRule (stringsP ["\\", "λ"])  [||WithToken do Token.SymLambda||]
    initialRule (stringP "|")           [||WithToken do Token.SymOr||]
    initialRule (stringP "~")           [||WithToken do Token.SymTilde||]

specialRules :: ScannerBuilder ()
specialRules = do
    initialRule (stringP "(")   [||WithToken do Token.SpParenOpen||]
    initialRule (stringP ")")   [||WithToken do Token.SpParenClose||]
    initialRule (stringP ",")   [||WithToken do Token.SpComma||]
    initialRule (stringP "[")   [||WithToken do Token.SpBrackOpen||]
    initialRule (stringP "]")   [||WithToken do Token.SpBrackClose||]
    initialRule (stringP "`")   [||WithToken do Token.SpBackquote||]
    initialRule (stringP ";")   [||WithToken do Token.SpSemi||]

specialCs = charsCs ['(', ')', ',', '[', ']', '`', ';']

braceRules :: ScannerBuilder ()
braceRules = do
    initialRule (stringsP ["{{", "❴"]) [||WithToken do Token.SpDBraceOpen||]
    initialRule (stringsP ["}}", "❵"]) [||WithToken do Token.SpDBraceClose||]
    initialRule (stringP "{")          [||WithToken do Token.SpBraceOpen||]
    initialRule (stringP "}")          [||WithToken do Token.SpBraceClose||]


literalRules :: ScannerBuilder ()
literalRules = do
    -- be before integerRules to avoid conflicting
    rationalRules
    integerRules

    -- lex rests without standard lexer
    initialRule byteStringOpenP         [||LexLitByteString||]
    initialRule stringOpenP             [||LexLitString||]
    initialRule byteCharOpenP           [||LexLitByteChar||]
    initialRule charOpenP               [||LexLitChar||]

    interpStringPartRules


rationalRules :: ScannerBuilder ()
rationalRules = do
    initialRule rationalWithDotP       [||LexLitRationalWithDot||]
    initialRule rationalWithoutDotP    [||LexLitRationalWithoutDot||]

integerRules :: ScannerBuilder ()
integerRules = do
    initialRule bitIntegerP            [||LexLitBitInteger||]
    initialRule octitIntegerP          [||LexLitOctitInteger||]
    initialRule hexitIntegerP          [||LexLitHexitInteger||]
    initialRule decimalIntegerP        [||LexLitDecimalInteger||]

rationalWithDotP =
    maySignP <> decimalP <> chP '.' <> decimalP <> Tlex.maybeP exponentP

rationalWithoutDotP = maySignP <> decimalP <> exponentP

bitIntegerP = maySignP <> zeroP <> charsP ['b', 'B']
    <> bitP <> Tlex.manyP do Tlex.orP [bitP, chP '_']
octitIntegerP = maySignP <> zeroP <> charsP ['o', 'O']
    <> octitP <> Tlex.manyP do Tlex.orP [octitP, chP '_']
hexitIntegerP = maySignP <> zeroP <> charsP ['x', 'X']
    <> hexitP <> Tlex.manyP do Tlex.orP [hexitP, chP '_']
decimalIntegerP = maySignP <> decimalP

decimalP = digitP <> Tlex.manyP do Tlex.orP [digitP, chP '_']

maySignP = Tlex.maybeP signP

signP = charSetP signCs
signCs = charsCs ['+', '-']

zeroP = charSetP zeroCs
zeroCs = charsCs ['0']

exponentP = charsP ['e', 'E'] <> maySignP <> decimalP

bitP = charSetP bitCs
bitCs = charsCs ['0', '1']

octitP = charSetP octitCs
octitCs = charsCs ['0', '1', '2', '3', '4', '5', '6', '7']

hexitP = charSetP octitCs
hexitCs = mconcat
    [
        digitCs,
        charsCs ['a', 'b', 'c', 'd', 'e', 'f'],
        charsCs ['A', 'B', 'C', 'D', 'E', 'F']
    ]


byteStringOpenP = splitOpenP <> chP 'r' <> strSepP

stringOpenP = strSepP

byteCharOpenP = splitOpenP <> chP 'r' <> charSepP

charOpenP = charSepP

splitOpenP = chP '#'

strSepP = charSetP strSepCs
strSepCs = charsCs ['"']

charSepP = charSepP charSepCs
charSepCs = charsCs ['\'']

escapeOpenP = charSetP escapeOpenCs
escapeOpenCs = charsCs ['\\']

escapeRule :: Pattern -> TH.Q (TH.TExp Char) -> CharEscScannerBuilder ()
escapeRule p = TlexTH.thLexRule [Initial] do escapeOpenP <> p

charEscRules :: CharEscScannerBuilder ()
charEscRules = do
    byteEscapeRules
    uniEscapeRule
    gapRule

byteEscapeRules :: CharEscScannerBuilder ()
byteEscapeRules = do
    charescRules
    asciiescRules

    escapeRule byteescP [||LexByteesc||]

uniEscapeRule :: CharEscScannerBuilder ()
uniEscapeRule = do
    escapeRule (stringP "u{" <> Tlex.someP hexitP <> stringP "}") [||LexUniEscape||]

gapRule :: CharEscScannerBuilder ()
gapRule = do
    escapeRule (chP '|' <> Tlex.manyP whiteCharP <> chP '|') [||WithGap||]

charescRules :: CharEscScannerBuilder ()
charescRules = do
    escapeRule (stringP "0") [||WithCharesc do fromEnum '\0'||]
    escapeRule (stringP "a") [||WithCharesc do fromEnum '\a'||]
    escapeRule (stringP "b") [||WithCharesc do fromEnum '\b'||]
    escapeRule (stringP "f") [||WithCharesc do fromEnum '\f'||]
    escapeRule (stringP "n") [||WithCharesc do fromEnum '\n'||]
    escapeRule (stringP "r") [||WithCharesc do fromEnum '\r'||]
    escapeRule (stringP "t") [||WithCharesc do fromEnum '\t'||]
    escapeRule (stringP "v") [||WithCharesc do fromEnum '\v'||]
    escapeRule (stringP "$") [||WithCharesc do fromEnum '$'||]
    escapeRule (stringP "\\") [||WithCharesc do fromEnum '\\'||]
    escapeRule (stringP "\"") [||WithCharesc do fromEnum '"'||]
    escapeRule (stringP "'") [||WithCharesc do fromEnum '\''||]

asciiescRules :: CharEscScannerBuilder ()
asciiescRules = do
    escapeRule (stringP "^A")   [||WithCharesc do fromEnum '\^A'||]
    escapeRule (stringP "^B")   [||WithCharesc do fromEnum '\^B'||]
    escapeRule (stringP "^C")   [||WithCharesc do fromEnum '\^C'||]
    escapeRule (stringP "^D")   [||WithCharesc do fromEnum '\^D'||]
    escapeRule (stringP "^E")   [||WithCharesc do fromEnum '\^E'||]
    escapeRule (stringP "^F")   [||WithCharesc do fromEnum '\^F'||]
    escapeRule (stringP "^G")   [||WithCharesc do fromEnum '\^G'||]
    escapeRule (stringP "^H")   [||WithCharesc do fromEnum '\^H'||]
    escapeRule (stringP "^I")   [||WithCharesc do fromEnum '\^I'||]
    escapeRule (stringP "^J")   [||WithCharesc do fromEnum '\^J'||]
    escapeRule (stringP "^K")   [||WithCharesc do fromEnum '\^K'||]
    escapeRule (stringP "^L")   [||WithCharesc do fromEnum '\^L'||]
    escapeRule (stringP "^M")   [||WithCharesc do fromEnum '\^M'||]
    escapeRule (stringP "^N")   [||WithCharesc do fromEnum '\^N'||]
    escapeRule (stringP "^O")   [||WithCharesc do fromEnum '\^O'||]
    escapeRule (stringP "^P")   [||WithCharesc do fromEnum '\^P'||]
    escapeRule (stringP "^Q")   [||WithCharesc do fromEnum '\^Q'||]
    escapeRule (stringP "^R")   [||WithCharesc do fromEnum '\^R'||]
    escapeRule (stringP "^S")   [||WithCharesc do fromEnum '\^S'||]
    escapeRule (stringP "^T")   [||WithCharesc do fromEnum '\^T'||]
    escapeRule (stringP "^U")   [||WithCharesc do fromEnum '\^U'||]
    escapeRule (stringP "^V")   [||WithCharesc do fromEnum '\^V'||]
    escapeRule (stringP "^W")   [||WithCharesc do fromEnum '\^W'||]
    escapeRule (stringP "^X")   [||WithCharesc do fromEnum '\^X'||]
    escapeRule (stringP "^Y")   [||WithCharesc do fromEnum '\^Y'||]
    escapeRule (stringP "^Z")   [||WithCharesc do fromEnum '\^Z'||]
    escapeRule (stringP "^@")   [||WithCharesc do fromEnum '\^@'||]
    escapeRule (stringP "^[")   [||WithCharesc do fromEnum '\^['||]
    escapeRule (stringP "^\\")  [||WithCharesc do fromEnum '\^\'||]
    escapeRule (stringP "^]")   [||WithCharesc do fromEnum '\^]'||]
    escapeRule (stringP "^^")   [||WithCharesc do fromEnum '\^^'||]
    escapeRule (stringP "^_")   [||WithCharesc do fromEnum '\^_'||]
    escapeRule (stringP "NUL")  [||WithCharesc do fromEnum '\NUL'||]
    escapeRule (stringP "SOH")  [||WithCharesc do fromEnum '\SOH'||]
    escapeRule (stringP "STX")  [||WithCharesc do fromEnum '\STX'||]
    escapeRule (stringP "ETX")  [||WithCharesc do fromEnum '\ETX'||]
    escapeRule (stringP "EOT")  [||WithCharesc do fromEnum '\EOT'||]
    escapeRule (stringP "ENQ")  [||WithCharesc do fromEnum '\ENQ'||]
    escapeRule (stringP "ACK")  [||WithCharesc do fromEnum '\ACK'||]
    escapeRule (stringP "BEL")  [||WithCharesc do fromEnum '\BEL'||]
    escapeRule (stringP "BS")   [||WithCharesc do fromEnum '\BS'||]
    escapeRule (stringP "HT")   [||WithCharesc do fromEnum '\HT'||]
    escapeRule (stringP "LF")   [||WithCharesc do fromEnum '\LF'||]
    escapeRule (stringP "VT")   [||WithCharesc do fromEnum '\VT'||]
    escapeRule (stringP "FF")   [||WithCharesc do fromEnum '\FF'||]
    escapeRule (stringP "CR")   [||WithCharesc do fromEnum '\CR'||]
    escapeRule (stringP "SO")   [||WithCharesc do fromEnum '\SO'||]
    escapeRule (stringP "SI")   [||WithCharesc do fromEnum '\SI'||]
    escapeRule (stringP "DLE")  [||WithCharesc do fromEnum '\DLE'||]
    escapeRule (stringP "DC1")  [||WithCharesc do fromEnum '\DC1'||]
    escapeRule (stringP "DC2")  [||WithCharesc do fromEnum '\DC2'||]
    escapeRule (stringP "DC3")  [||WithCharesc do fromEnum '\DC3'||]
    escapeRule (stringP "DC4")  [||WithCharesc do fromEnum '\DC4'||]
    escapeRule (stringP "NAK")  [||WithCharesc do fromEnum '\NAK'||]
    escapeRule (stringP "SYN")  [||WithCharesc do fromEnum '\SYN'||]
    escapeRule (stringP "ETB")  [||WithCharesc do fromEnum '\ETB'||]
    escapeRule (stringP "CAN")  [||WithCharesc do fromEnum '\CAN'||]
    escapeRule (stringP "EM")   [||WithCharesc do fromEnum '\EM'||]
    escapeRule (stringP "SUB")  [||WithCharesc do fromEnum '\SUB'||]
    escapeRule (stringP "ESC")  [||WithCharesc do fromEnum '\ESC'||]
    escapeRule (stringP "FS")   [||WithCharesc do fromEnum '\FS'||]
    escapeRule (stringP "GS")   [||WithCharesc do fromEnum '\GS'||]
    escapeRule (stringP "RS")   [||WithCharesc do fromEnum '\RS'||]
    escapeRule (stringP "US")   [||WithCharesc do fromEnum '\US'||]
    escapeRule (stringP "SP")   [||WithCharesc do fromEnum '\SP'||]
    escapeRule (stringP "DEL")  [||WithCharesc do fromEnum '\DEL'||]

byteescP = chP 'x' <> hexitP <> hexitP


interpStringPartRules :: ScannerBuilder ()
interpStringPartRules = do
    -- lex rests without standard lexer
    initialRule interpStringStartP      [||LexInterpStringStart||]
    initialRule interpStringContinueP   [||LexInterpStringContinue||]

interpStringStartP = splitOpenP <> chP 's' <> strSepP

interpStringContinueP = stringsP ["#}", "⦄"]


whiteSpaceRules :: ScannerBuilder ()
whiteSpaceRules = do
    initialRule (Tlex.someP whiteCharP) [||WithWhitespace||]

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
        CodeUnit.catDecimalNumber
    ]

otherP = charSetP otherCs
otherCs = mconcat
    [
        charsCs ['\''],
        CodeUnit.catModifierLetter,
        CodeUnit.catMark,
        CodeUnit.catLetterNumber,
        CodeUnit.catOtherNumber,
        CodeUnit.catFormat `csDifference` whiteCharCs
    ]

otherSpecialCs = charsCs
    ['#', '"', '{', '}', '⦃', '⦄', '❴', '❵']

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

charsP :: [Char] -> Pattern
charsP cs = charSetP do charsCs cs

stringP :: StringLit -> Pattern
stringP s = foldMap chP s

stringsP :: [StringLit] -> Pattern
stringsP ss = Tlex.orP [stringP s | s <- ss]
