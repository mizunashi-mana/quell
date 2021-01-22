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
                        byteString "01 +103-40.0a0,40e10o0]"
                    ]
            rs `shouldBe` mempty
            ts `shouldBe`
                [
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 0, locCol = 0, locBytesPos = 0},
                            endLoc = Loc {locLine = 0, locCol = 1, locBytesPos = 1}
                        },
                        unSpanned = SpBrackOpen
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 0, locCol = 1, locBytesPos = 1},
                            endLoc = Loc {locLine = 0, locCol = 3, locBytesPos = 3}
                        },
                        unSpanned = IdVarId (TextId.stringLit "id")
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 0, locBytesPos = 4},
                            endLoc = Loc {locLine = 1, locCol = 1, locBytesPos = 5}
                        },
                        unSpanned = IdVarId (TextId.stringLit "x")
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 1, locBytesPos = 5},
                            endLoc = Loc {locLine = 1, locCol = 2, locBytesPos = 6}
                        },
                        unSpanned = SpParenOpen
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 2, locBytesPos = 6},
                            endLoc = Loc {locLine = 1, locCol = 5, locBytesPos = 9}
                        },
                        unSpanned = LitInteger 0
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 5, locBytesPos = 9},
                            endLoc = Loc {locLine = 1, locCol = 6, locBytesPos = 10}
                        },
                        unSpanned = SpParenClose
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 6, locBytesPos = 10},
                            endLoc = Loc {locLine = 1, locCol = 10, locBytesPos = 14}
                        },
                        unSpanned = IdVarId (TextId.stringLit "aa00")
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 10, locBytesPos = 14},
                            endLoc = Loc {locLine = 1, locCol = 11, locBytesPos = 15}
                        },
                        unSpanned = SpComma
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 11, locBytesPos = 15},
                            endLoc = Loc {locLine = 1, locCol = 14, locBytesPos = 18}
                        },
                        unSpanned = LitInteger 101
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 15, locBytesPos = 19},
                            endLoc = Loc {locLine = 1, locCol = 19, locBytesPos = 23}
                        },
                        unSpanned = LitInteger 103
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 19, locBytesPos = 23},
                            endLoc = Loc {locLine = 1, locCol = 24, locBytesPos = 28}
                        },
                        unSpanned = LitRational ((-40) % 1)
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 24, locBytesPos = 28},
                            endLoc = Loc {locLine = 1, locCol = 26, locBytesPos = 30}
                        },
                        unSpanned = IdVarId (TextId.stringLit "a0")
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 26, locBytesPos = 30},
                            endLoc = Loc {locLine = 1, locCol = 27, locBytesPos = 31}
                        },
                        unSpanned = SpComma
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 27, locBytesPos = 31},
                            endLoc = Loc {locLine = 1, locCol = 32, locBytesPos = 36}
                        },
                        unSpanned = LitRational (400000000000 % 1)
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 32, locBytesPos = 36},
                            endLoc = Loc {locLine = 1, locCol = 34, locBytesPos = 38}
                        },
                        unSpanned = IdVarId (TextId.stringLit "o0")
                    },
                    Spanned {
                        getSpan = Span {
                            beginLoc = Loc {locLine = 1, locCol = 34, locBytesPos = 38},
                            endLoc = Loc {locLine = 1, locCol = 35, locBytesPos = 39}
                        },
                        unSpanned = SpBrackClose
                    }
                ]

textId :: StringLit -> TextId.T
textId str = TextId.textId do text str

lexFromList :: [ByteString] -> (Bag.T LexerReport, [Spanned Token])
lexFromList bss = runTestLexer do
    Conduit.runConduit do
        ConduitList.sourceList bss
            Conduit..| lexerConduit Encoding.EncodingUtf8
            Conduit..| ConduitList.consume
