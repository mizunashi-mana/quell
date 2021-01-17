module Language.Quell.Parsing.Lexer.EncodingSpec (spec) where

import           Language.Quell.Prelude
import           Test.Hspec

import           Language.Quell.Parsing.Lexer.Encoding

import           Language.Quell.Parsing.Spanned
import qualified Conduit
import qualified Data.Conduit.List as ConduitList
import qualified Data.ByteString as ByteString


spec :: Spec
spec = do
    describe "decodeUtf8Conduit" do
        it "decode ASC-II strings" do
            let decodedUnits = runDecodeUtf8Conduit
                    [
                        byteString "abcde012\n",
                        byteString "\rABC"
                    ]
            decodedUnits `shouldBe`
                [
                    (BytesSpan {bytesIndex = 0, bytesLength = 1},DecodedChar 'a'),
                    (BytesSpan {bytesIndex = 1, bytesLength = 1},DecodedChar 'b'),
                    (BytesSpan {bytesIndex = 2, bytesLength = 1},DecodedChar 'c'),
                    (BytesSpan {bytesIndex = 3, bytesLength = 1},DecodedChar 'd'),
                    (BytesSpan {bytesIndex = 4, bytesLength = 1},DecodedChar 'e'),
                    (BytesSpan {bytesIndex = 5, bytesLength = 1},DecodedChar '0'),
                    (BytesSpan {bytesIndex = 6, bytesLength = 1},DecodedChar '1'),
                    (BytesSpan {bytesIndex = 7, bytesLength = 1},DecodedChar '2'),
                    (BytesSpan {bytesIndex = 8, bytesLength = 1},DecodedChar '\n'),
                    (BytesSpan {bytesIndex = 9, bytesLength = 1},DecodedChar '\r'),
                    (BytesSpan {bytesIndex = 10, bytesLength = 1},DecodedChar 'A'),
                    (BytesSpan {bytesIndex = 11, bytesLength = 1},DecodedChar 'B'),
                    (BytesSpan {bytesIndex = 12, bytesLength = 1},DecodedChar 'C')
                ]

        it "decode UTF-8 strings" do
            let decodedUnits = runDecodeUtf8Conduit
                    [
                        byteString "abc1",
                        ByteString.pack [0x32, 0x33, 0x0A, 0xE3],
                        ByteString.pack [0x81, 0x82, 0xE3, 0x81, 0x84, 0xE3, 0x81],
                        ByteString.pack [0x86, 0xEF, 0xBC, 0x91, 0xEF, 0xBC, 0x92, 0xEF, 0xBC, 0x93],
                        ByteString.pack [0xF0, 0x9F, 0x98, 0x84]
                    ]
            decodedUnits `shouldBe`
                [
                    (BytesSpan {bytesIndex = 0, bytesLength = 1},DecodedChar 'a'),
                    (BytesSpan {bytesIndex = 1, bytesLength = 1},DecodedChar 'b'),
                    (BytesSpan {bytesIndex = 2, bytesLength = 1},DecodedChar 'c'),
                    (BytesSpan {bytesIndex = 3, bytesLength = 1},DecodedChar '1'),
                    (BytesSpan {bytesIndex = 4, bytesLength = 1},DecodedChar '2'),
                    (BytesSpan {bytesIndex = 5, bytesLength = 1},DecodedChar '3'),
                    (BytesSpan {bytesIndex = 6, bytesLength = 1},DecodedChar '\n'),
                    (BytesSpan {bytesIndex = 7, bytesLength = 3},DecodedChar 'あ'),
                    (BytesSpan {bytesIndex = 10, bytesLength = 3},DecodedChar 'い'),
                    (BytesSpan {bytesIndex = 13, bytesLength = 3},DecodedChar 'う'),
                    (BytesSpan {bytesIndex = 16, bytesLength = 3},DecodedChar '１'),
                    (BytesSpan {bytesIndex = 19, bytesLength = 3},DecodedChar '２'),
                    (BytesSpan {bytesIndex = 22, bytesLength = 3},DecodedChar '３'),
                    (BytesSpan {bytesIndex = 25, bytesLength = 4},DecodedChar '😄')
                ]

        it "decode partial UTF-8 strings with illegal codes" do
            let decodedUnits = runDecodeUtf8Conduit
                    [
                        byteString "abc1",
                        ByteString.pack [0x32, 0x33, 0x0A, 0x80, 0xE3],
                        ByteString.pack [0x81, 0x82, 0xE3, 0xE3, 0x84, 0xE3, 0x81],
                        ByteString.pack [0x86, 0xEF, 0xBC, 0x91, 0xEF, 0xBC, 0x92, 0xEF, 0xBC, 0x93],
                        ByteString.pack [0xED, 0xB1, 0x90, 0xF0, 0x9F, 0x98, 0x84, 0xBF]
                    ]
            decodedUnits `shouldBe`
                [
                    (BytesSpan {bytesIndex = 0, bytesLength = 1},DecodedChar 'a'),
                    (BytesSpan {bytesIndex = 1, bytesLength = 1},DecodedChar 'b'),
                    (BytesSpan {bytesIndex = 2, bytesLength = 1},DecodedChar 'c'),
                    (BytesSpan {bytesIndex = 3, bytesLength = 1},DecodedChar '1'),
                    (BytesSpan {bytesIndex = 4, bytesLength = 1},DecodedChar '2'),
                    (BytesSpan {bytesIndex = 5, bytesLength = 1},DecodedChar '3'),
                    (BytesSpan {bytesIndex = 6, bytesLength = 1},DecodedChar '\n'),
                    (BytesSpan {bytesIndex = 7, bytesLength = 1},DecodeError do text "masked"),
                    (BytesSpan {bytesIndex = 8, bytesLength = 3},DecodedChar 'あ'),
                    (BytesSpan {bytesIndex = 11, bytesLength = 3},DecodeError do text "masked"),
                    (BytesSpan {bytesIndex = 14, bytesLength = 3},DecodedChar 'う'),
                    (BytesSpan {bytesIndex = 17, bytesLength = 3},DecodedChar '１'),
                    (BytesSpan {bytesIndex = 20, bytesLength = 3},DecodedChar '２'),
                    (BytesSpan {bytesIndex = 23, bytesLength = 3},DecodedChar '３'),
                    -- surrogate code point
                    (BytesSpan {bytesIndex = 26, bytesLength = 3},DecodeError do text "masked"),
                    (BytesSpan {bytesIndex = 29, bytesLength = 4},DecodedChar '😄'),
                    (BytesSpan {bytesIndex = 33, bytesLength = 1},DecodeError do text "masked")
                ]

runDecodeUtf8Conduit :: [ByteString] -> [(BytesSpan, DecodedUnit)]
runDecodeUtf8Conduit bs = Conduit.runConduitPure do
    ConduitList.sourceList bs
        Conduit..| decodeUtf8Conduit
        Conduit..| maskDecodedUnitConduit
        Conduit..| ConduitList.consume

maskDecodedUnitConduit :: forall m. Monad m =>
    Conduit.ConduitT
        (BytesSpan, DecodedUnit)
        (BytesSpan, DecodedUnit)
        m ()
maskDecodedUnitConduit = go
    where
        go :: Conduit.ConduitT
            (BytesSpan, DecodedUnit)
            (BytesSpan, DecodedUnit)
            m ()
        go = Conduit.await >>= \case
            Nothing ->
                pure ()
            Just (bs, u) -> do
                Conduit.yield (bs, maskDecodedUnit u)
                go

        maskDecodedUnit = \case
            DecodedChar c -> DecodedChar c
            DecodeError _ -> DecodeError do text "masked"
