module Language.Quell.Parsing.Lexer.Encoding (
    T,
    DecodedUnit (..),
    Encoding (..),
    decodeConduit,
    decodeUtf8Conduit,
) where

import           Language.Quell.Prelude

import qualified Conduit
import qualified Data.Bits as Bits
import qualified Language.Quell.Parsing.Spanned as Spanned


type T = Encoding

data Encoding
    = EncodingUtf8
    deriving (Eq, Ord, Enum, Bounded, Show)

data DecodedUnit
    = DecodedChar Char
    | DecodeError Text
    deriving (Eq, Show)

type BytesSpannedUnit = (Spanned.BytesSpan, DecodedUnit)

decodeConduit :: Monad m => Encoding
    -> Conduit.ConduitT ByteString BytesSpannedUnit m ()
decodeConduit = \case
    EncodingUtf8 -> decodeUtf8Conduit

decodeUtf8Conduit :: Monad m
    => Conduit.ConduitT ByteString BytesSpannedUnit m ()
decodeUtf8Conduit = go ctx0 where
    ctx0 = DecodeUtf8Context
        {
            bufferByteString = mempty,
            currentBytesCount = 0
        }

    go ctx = case headMay do bufferByteString ctx of
        Just w
            | w <= 0x7F ->
                goConsume 0 ctx w
            | 0xC2 <= w && w <= 0xDF ->
                goConsume 1 ctx
                    do w Bits..&. 0b11111
            | 0xE0 <= w && w <= 0xEF ->
                goConsume 2 ctx
                    do w Bits..&. 0b1111
            | 0xF0 <= w && w <= 0xF4 ->
                goConsume 3 ctx
                    do w Bits..&. 0b111
            | otherwise -> do
                yieldError "Found an illegal first byte." ctx 1
                go do consumeBuffer 1 ctx
        Nothing -> Conduit.await >>= \case
            Nothing ->
                pure ()
            Just bs ->
                go do ctx { bufferByteString = bs }

    goConsume n ctx b = do
        let bi = fromIntegral b :: Int
            ctx1 = consumeBuffer 1 ctx
        consumeAndAwaitBuffer bi n 0 ctx1 >>= \case
            Left (m, mctx2) -> do
                let consumed = 1 + m
                if
                    | m < n ->
                        yieldError "Contents is not enough." ctx consumed
                    | otherwise ->
                        yieldError "Found a ill-formed sequence." ctx consumed
                case mctx2 of
                    Nothing ->
                        pure ()
                    Just ctx2 ->
                        go ctx2
            Right (c, m, ctx2) -> do
                if
                    | 0xD800 <= c && c <= 0xDFFF ->
                        yieldError "Not allowed surrogate code points." ctx m
                    -- @c <= 0x7F => n == 0@ is always satisfied.
                    | c <= 0x7FF && n > 1 ->
                        yieldError "Found a non-shortest form." ctx m
                    | c <= 0xFFFF && n > 2 ->
                        yieldError "Found a non-shortest form." ctx m
                    | otherwise ->
                        yieldByInt c ctx m
                go ctx2

    consumeAndAwaitBuffer b n m ctx = case n of
        0 -> pure do Right (b, m, ctx)
        _ -> case headMay do bufferByteString ctx of
            Just w
                | w <= 0x7F ->
                    consumeAndAwaitBufferWithError m n ctx
                | otherwise ->
                    consumeAndAwaitBuffer
                        do b * 0x100 + fromIntegral do w Bits..&. 0x7F
                        do n - 1
                        do m + 1
                        do consumeBuffer 1 ctx
            Nothing -> Conduit.await >>= \case
                Nothing ->
                    pure do Left (m, Nothing)
                Just bs ->
                    consumeAndAwaitBuffer b n m
                        do ctx { bufferByteString = bs }

    consumeAndAwaitBufferWithError m n ctx =
        let bufferLen = olength do bufferByteString ctx
        in if
            | n <= bufferLen ->
                pure do Left (m + n, Just do consumeBuffer n ctx)
            | otherwise -> Conduit.await >>= \case
                Nothing ->
                    pure do Left (m + bufferLen, Nothing)
                Just bs ->
                    consumeAndAwaitBufferWithError
                        do m + bufferLen
                        do n - bufferLen
                        do ctx
                            {
                                bufferByteString = bs,
                                currentBytesCount = bufferLen + currentBytesCount ctx
                            }

    consumeBuffer n ctx = ctx
        {
            bufferByteString = drop n do bufferByteString ctx,
            currentBytesCount = n + currentBytesCount ctx
        }

    yieldByInt i ctx n = Conduit.yield
        (
            Spanned.BytesSpan {
                bytesIndex = currentBytesCount ctx,
                bytesLength = n
            },
            DecodedChar do toEnum i
        )

    yieldError msg ctx n = Conduit.yield
        (
            Spanned.BytesSpan {
                bytesIndex = currentBytesCount ctx,
                bytesLength = n
            },
            DecodeError do text msg
        )

data DecodeUtf8Context = DecodeUtf8Context
    {
        bufferByteString :: ByteString,
        currentBytesCount :: Int
    }
