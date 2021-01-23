module Language.Quell.Parsing.LexerSpec where

import           Language.Quell.Prelude
import           Test.Hspec

import           Language.Quell.Parsing.Lexer
import           Language.Quell.Parsing.Spanned
import           Language.Quell.Type.Token

import qualified Conduit
import qualified Data.Conduit.List                     as ConduitList
import qualified Language.Quell.Data.Bag               as Bag
import qualified Language.Quell.Data.Monad.MonadST     as MonadST
import qualified Language.Quell.Data.TextId            as TextId
import qualified Language.Quell.Parsing.Lexer.Encoding as Encoding
import qualified Language.Quell.Parsing.Lexer.Error    as Error


data LexerReport
    = ReportDecodeError BytesSpan Text
    | ReportLexError Span Error.T Text
    deriving (Eq, Show)

newtype TestLexer s a = TestLexer
    {
        unTestLexer :: ST s (Bag.T LexerReport, a)
    }
    deriving Functor

runTestLexer :: (forall s. TestLexer s a) -> (Bag.T LexerReport, a)
runTestLexer l = runST do unTestLexer l

instance Applicative (TestLexer s) where
    pure x = TestLexer do pure do pure x
    mf <*> mx = TestLexer do
        (rs1, f) <- unTestLexer mf
        (rs2, x) <- unTestLexer mx
        pure (rs1 <> rs2, f x)

instance Monad (TestLexer s) where
    mx >>= f = TestLexer do
        (rs1, x) <- unTestLexer mx
        (rs2, fx) <- unTestLexer do f x
        pure (rs1 <> rs2, fx)

instance MonadST.MonadST s (TestLexer s) where
    type Marker (TestLexer s) = s
    liftST mx = TestLexer do mx <&> \x -> (mempty, x)

instance LexerMonad s (TestLexer s) where
    reportDecodeError bsp txt = TestLexer do
        pure (pure do ReportDecodeError bsp txt, ())
    reportLexError sp err txt = TestLexer do
        pure (pure do ReportLexError sp err txt, ())

spec :: Spec
spec = do
    describe "lexerConduit" do
        it "returns tokens without any reports" do
            let (rs, ts) = lexFromList
                    [
                        byteString "[id\nx(0b0)aa00,1",
                        byteString "01 +103-40.0a0,40e10o0]",
                        byteString "(#r\"a\\x00\" '\\u{11}')"
                    ]
            rs `shouldBe` mempty
            ts `shouldBe`
                [
                    Spanned {getSpan = Span {beginLoc = Loc {locBytesPos = 0, locLine = 0, locCol = 0}, endLoc = Loc {locBytesPos = 1, locLine = 0, locCol = 1}}, unSpanned = SpBrackOpen},
                    Spanned {getSpan = Span {beginLoc = Loc {locBytesPos = 2, locLine = 0, locCol = 2}, endLoc = Loc {locBytesPos = 3, locLine = 0, locCol = 3}}, unSpanned = IdVarId (textId "id")},
                    Spanned {getSpan = Span {beginLoc = Loc {locBytesPos = 4, locLine = 1, locCol = 0}, endLoc = Loc {locBytesPos = 5, locLine = 1, locCol = 1}}, unSpanned = IdVarId (textId "x")},
                    Spanned {getSpan = Span {beginLoc = Loc {locBytesPos = 5, locLine = 1, locCol = 1}, endLoc = Loc {locBytesPos = 6, locLine = 1, locCol = 2}}, unSpanned = SpParenOpen},
                    Spanned {getSpan = Span {beginLoc = Loc {locBytesPos = 8, locLine = 1, locCol = 4}, endLoc = Loc {locBytesPos = 9, locLine = 1, locCol = 5}}, unSpanned = LitInteger 0},
                    Spanned {getSpan = Span {beginLoc = Loc {locBytesPos = 9, locLine = 1, locCol = 5}, endLoc = Loc {locBytesPos = 10, locLine = 1, locCol = 6}}, unSpanned = SpParenClose},
                    Spanned {getSpan = Span {beginLoc = Loc {locBytesPos = 13, locLine = 1, locCol = 9}, endLoc = Loc {locBytesPos = 14, locLine = 1, locCol = 10}}, unSpanned = IdVarId (textId "aa00")},
                    Spanned {getSpan = Span {beginLoc = Loc {locBytesPos = 14, locLine = 1, locCol = 10}, endLoc = Loc {locBytesPos = 15, locLine = 1, locCol = 11}}, unSpanned = SpComma},
                    Spanned {getSpan = Span {beginLoc = Loc {locBytesPos = 17, locLine = 1, locCol = 13}, endLoc = Loc {locBytesPos = 18, locLine = 1, locCol = 14}}, unSpanned = LitInteger 101},
                    Spanned {getSpan = Span {beginLoc = Loc {locBytesPos = 22, locLine = 1, locCol = 18}, endLoc = Loc {locBytesPos = 23, locLine = 1, locCol = 19}}, unSpanned = LitInteger 103},
                    Spanned {getSpan = Span {beginLoc = Loc {locBytesPos = 27, locLine = 1, locCol = 23}, endLoc = Loc {locBytesPos = 28, locLine = 1, locCol = 24}}, unSpanned = LitRational ((-40) % 1)},
                    Spanned {getSpan = Span {beginLoc = Loc {locBytesPos = 29, locLine = 1, locCol = 25}, endLoc = Loc {locBytesPos = 30, locLine = 1, locCol = 26}}, unSpanned = IdVarId (textId "a0")},
                    Spanned {getSpan = Span {beginLoc = Loc {locBytesPos = 30, locLine = 1, locCol = 26}, endLoc = Loc {locBytesPos = 31, locLine = 1, locCol = 27}}, unSpanned = SpComma},
                    Spanned {getSpan = Span {beginLoc = Loc {locBytesPos = 35, locLine = 1, locCol = 31}, endLoc = Loc {locBytesPos = 36, locLine = 1, locCol = 32}}, unSpanned = LitRational (400000000000 % 1)},
                    Spanned {getSpan = Span {beginLoc = Loc {locBytesPos = 37, locLine = 1, locCol = 33}, endLoc = Loc {locBytesPos = 38, locLine = 1, locCol = 34}}, unSpanned = IdVarId (textId "o0")},
                    Spanned {getSpan = Span {beginLoc = Loc {locBytesPos = 38, locLine = 1, locCol = 34}, endLoc = Loc {locBytesPos = 39, locLine = 1, locCol = 35}}, unSpanned = SpBrackClose},
                    Spanned {getSpan = Span {beginLoc = Loc {locBytesPos = 39, locLine = 1, locCol = 35}, endLoc = Loc {locBytesPos = 40, locLine = 1, locCol = 36}}, unSpanned = SpParenOpen},
                    Spanned {getSpan = Span {beginLoc = Loc {locBytesPos = 48, locLine = 1, locCol = 44}, endLoc = Loc {locBytesPos = 49, locLine = 1, locCol = 45}}, unSpanned = LitByteString do byteString "a\NUL"},
                    Spanned {getSpan = Span {beginLoc = Loc {locBytesPos = 57, locLine = 1, locCol = 53}, endLoc = Loc {locBytesPos = 58, locLine = 1, locCol = 54}}, unSpanned = LitChar '\DC1'},
                    Spanned {getSpan = Span {beginLoc = Loc {locBytesPos = 58, locLine = 1, locCol = 54}, endLoc = Loc {locBytesPos = 59, locLine = 1, locCol = 55}}, unSpanned = SpParenClose}
                ]

textId :: StringLit -> TextId.T
textId str = TextId.textId do text str

lexFromList :: [ByteString] -> (Bag.T LexerReport, [Spanned Token])
lexFromList bss = runTestLexer do
    Conduit.runConduit do
        ConduitList.sourceList bss
            Conduit..| lexerConduit Encoding.EncodingUtf8
            Conduit..| ConduitList.consume
