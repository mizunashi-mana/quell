module Language.Quell.Parsing.LexerSpec where

import           Language.Quell.Prelude
import           Test.Hspec

import           Language.Quell.Parsing.Lexer
import           Language.Quell.Parsing.Spanned

import qualified Conduit
import qualified Data.Conduit.List                     as ConduitList
import qualified Language.Quell.Data.Bag               as Bag
import qualified Language.Quell.Data.Monad.MonadST     as MonadST
import qualified Language.Quell.Data.TextId            as TextId
import qualified Language.Quell.Parsing.Lexer.Encoding as Encoding
import qualified Language.Quell.Parsing.Lexer.Error    as Error
import qualified Language.Quell.Type.Token             as Token


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
                        byteString "01 +103-40.0a0,40e10o0]"
                    ]
            rs `shouldBe` mempty
            ts `shouldBe`
                [
                    Spanned {
                        getSpan = Span
                            {
                                beginLoc = Loc {locLine = 0, locCol = 0, locBytesPos = 0},
                                endLoc = Loc {locLine = 0, locCol = 2, locBytesPos = 2}
                            },
                        unSpanned = Token.IdVarId do textId "id"
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 0, locBytesPos = 3},
                            endLoc = Loc {locLine = 1, locCol = 1, locBytesPos = 4}
                        },
                        unSpanned = Token.IdVarId do textId "x"
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 1, locBytesPos = 4},
                            endLoc = Loc {locLine = 1, locCol = 2, locBytesPos = 5}
                        },
                        unSpanned = Token.SpParenOpen
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 2, locBytesPos = 5},
                            endLoc = Loc {locLine = 1, locCol = 5, locBytesPos = 8}
                        },
                        unSpanned = Token.LitInteger 0
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 5, locBytesPos = 8},
                            endLoc = Loc {locLine = 1, locCol = 6, locBytesPos = 9}
                        },
                        unSpanned = Token.SpParenClose
                    }
                ]

        it "returns tokens without any reports (minimal)" do
            let (rs, ts) = lexFromList
                    [
                        byteString "0b0"
                    ]
            rs `shouldBe` mempty
            ts `shouldBe`
                [
                    Spanned {
                        getSpan = Span
                            {
                                beginLoc = Loc {locLine = 0, locCol = 0, locBytesPos = 0},
                                endLoc = Loc {locLine = 0, locCol = 2, locBytesPos = 2}
                            },
                        unSpanned = Token.IdVarId do textId "id"
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 0, locBytesPos = 3},
                            endLoc = Loc {locLine = 1, locCol = 1, locBytesPos = 4}
                        },
                        unSpanned = Token.IdVarId do textId "x"
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 1, locBytesPos = 4},
                            endLoc = Loc {locLine = 1, locCol = 2, locBytesPos = 5}
                        },
                        unSpanned = Token.SpParenOpen
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 2, locBytesPos = 5},
                            endLoc = Loc {locLine = 1, locCol = 5, locBytesPos = 8}
                        },
                        unSpanned = Token.LitInteger 0
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 5, locBytesPos = 8},
                            endLoc = Loc {locLine = 1, locCol = 6, locBytesPos = 9}
                        },
                        unSpanned = Token.SpParenClose
                    }
                ]

textId :: StringLit -> TextId.T
textId str = TextId.textId do text str

lexFromList :: [ByteString] -> (Bag.T LexerReport, [Spanned Token.T])
lexFromList bss = runTestLexer do
    Conduit.runConduit do
        ConduitList.sourceList bss
            Conduit..| lexerConduit Encoding.EncodingUtf8
            Conduit..| ConduitList.consume
