module Language.Quell.Type.Error (
  T,
  Error (..),
  DetailedError (..),
  detailedError,
  toErrorCode,

  detailedErrorUnknown,
) where

import           Language.Quell.Prelude

import qualified Prelude
import qualified Language.Quell.Type.ErrorCode as ErrorCode
import qualified Language.Quell.Parsing.Lexer.Encoding as LexerEncoding


type T = Error

data DetailedError = DetailedError
    {
        getError :: Error,
        detailedMessage :: Text,
        getCallStack :: Maybe CallStack
    }
    deriving (Eq, Show)

detailedError :: Text -> Error -> DetailedError
detailedError msg err = DetailedError
    {
        getError = err,
        detailedError = msg,
        getCallStack = Nothing
    }

data Error
    = Unknown
    | LexDecodeError LexerEncoding.BytesSpan
    deriving (Eq, Show)

toErrorCode :: Error -> ErrorCode.T
toErrorCode = \case
    Unknown             -> ErrorCode.Unknown
    LexDecodeError{}    -> ErrorCode.LexBreakEncoding

detailedErrorUnknown :: HasCallStack => Prelude.String -> DetailedError
detailedErrorUnknown msg =
    let cs = callStack
    in DetailedError
        {
            getError = Unknown,
            detailedError = text msg,
            getCallStack = Just cs
        }
