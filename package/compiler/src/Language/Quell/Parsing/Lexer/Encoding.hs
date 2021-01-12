module Language.Quell.Parsing.Lexer.Encoding (
    DecodedUnit (..),
    Encoding (..),
    decodeConduit,
    decodeUtf8Conduit,
) where

import           Language.Quell.Prelude

import qualified Conduit
import qualified Data.Bits as Bits


data DecodedUnit
    = DecodedChar Char
    | DecodeError Int Int

data Encoding
    = EncodingUtf8
    deriving (Eq, Ord, Enum, Bounded, Show)

decodeConduit :: Monad m => Encoding -> Conduit.ConduitT ByteString DecodeUnit m ()
decodeConduit = \case
    EncodingUtf8 -> decodeUtf8Conduit

decodeUtf8Conduit :: Monad m => Conduit.ConduitT ByteString DecodeUnit m ()
decodeUtf8Conduit = go ctx0 where
    ctx0 = DecodeUtf8Context
        {
            bufferByteString = mempty,
            currentBytesCount = 0
        }

    go ctx = case headMay do bufferByteString ctx of
        Just w
            | w <= 0x7F ->
                goConsume 0 ctx do fromIntegral w
            | 0xC2 <= w && w <= 0xDF ->
                goConsume 1 ctx
                    do fromIntegral do w Bits..&. 0b11111
            | 0xE0 <= w && w <= 0xEF ->
                goConsume 2 ctx
                    do fromIntegral do w Bits..&. 0b1111
            | 0xF0 <= w && w <= 0xF4 ->
                goConsume 3 ctx
                    do fromIntegral do w Bits..&. 0b111
            | otherwise -> do
                yieldError ctx 1
                go do consumeBuffer 1 ctx
        Nothing -> Conduit.await >>= \case
            Nothing ->
                pure ()
            Just bs ->
                go do ctx { bufferByteString = bs }

    goConsume b n ctx = do
        let ctx1 = consumeBuffer 1 ctx
        consumeAndAwaitBuffer b n 0 ctx1 >>= \case
            Left (m, mctx2) ->
                yieldError ctx do 1 + m
                case mctx2 of
                    Nothing ->
                        pure ()
                    Just ctx2 ->
                        go ctx2
            Right (c, ctx2)
                | 0xD800 <= c && c <= 0xDFFF ->
                    yieldError ctx 3
                    go ctx2
                | otherwise ->
                    yieldByInt c
                    go ctx2

    consumeAndAwaitBuffer b n m ctx = case n of
        0 -> Right (b, ctx)
        _ -> case headMay do bufferByteString ctx of
            Just w
                | w <= 0x7F ->
                    consumeAndAwaitBufferWithError m n ctx
                | otherwise ->
                    consumeAndAwaitBuffer
                        do b * 0x100 + w Bits..&. 0x7F
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
            | n <= bufferLen
                Left (m + n, Just do consumeBuffer n ctx)
            | otherwise -> Conduit.await >>= \case
                Nothing ->
                    Left (m + bufferLen, Nothing)
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

    yieldByInt i = Conduit.yield do DecodedChar do toEnum i

    yieldError ctx n = Conduit.yield do
        DecodeError
            do currentBytesCount ctx
            n

data DecodeUtf8Context = DecodeUtf8Context
    {
        bufferByteString :: ByteString,
        currentBytesCount :: Int
    }
