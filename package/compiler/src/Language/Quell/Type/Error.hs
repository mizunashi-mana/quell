module Language.Quell.Type.Error (
  T,
  Error (..),
  DetailedError (..),
  detailedError,
  toErrorCode,

  detailedErrorUnknown,
) where

import           Language.Quell.Prelude

import qualified Language.Quell.Type.ErrorCode as ErrorCode
import qualified Language.Quell.Parsing.Spanned as Spanned


type T = Error

data DetailedError = DetailedError
    {
        getError :: Error,
        detailedMessage :: Text,
        getCallStack :: Maybe CallStack
    }
    deriving Show

detailedError :: Text -> Error -> DetailedError
detailedError msg err = DetailedError
    {
        getError = err,
        detailedMessage = msg,
        getCallStack = Nothing
    }

data Error
    = Unknown
    | LexDecodeError Spanned.BytesSpan
    deriving (Eq, Show)

toErrorCode :: Error -> ErrorCode.T
toErrorCode = \case
    Unknown             -> ErrorCode.Unknown
    LexDecodeError{}    -> ErrorCode.LexBreakEncoding

detailedErrorUnknown :: HasCallStack => StringLit -> DetailedError
detailedErrorUnknown msg =
    let cs = callStack
    in DetailedError
        {
            getError = Unknown,
            detailedMessage = text msg,
            getCallStack = Just cs
        }
