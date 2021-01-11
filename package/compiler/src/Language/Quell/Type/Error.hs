module Language.Quell.Type.Error (
  Errorable,
  Error (..),
  errorCode,
  detailedMessage,

  errorUnknown,
) where

import           Language.Quell.Prelude

import qualified Language.Quell.Type.ErrorCode as ErrorCode


type ErrorConstraint code =
  (
    Show (Error code),
    Pretty (Error code),
    Typeable (Error code)
  )

class ErrorConstraint code => Errorable (code :: ErrorCode.T) where
  data Error code :: Type

  errorCode :: Error code -> ErrorCode.T

  detailedMessage :: Error code -> Maybe (Doc ann)


instance Errorable 'ErrorCode.Unknown where
  data Error 'ErrorCode.Unknown where
    ErrorUnknown :: CallStack -> Text -> Error 'ErrorCode.Unknown
    deriving (Show, Typeable)

  errorCode _ = ErrorCode.Unknown
  detailedMessage _ = Nothing -- FIXME

errorUnknown :: HasCallStack => Text -> Error 'ErrorCode.Unknown
errorUnknown msg = ErrorUnknown callStack msg

instance Pretty (Error 'ErrorCode.Unknown) where
  pretty = \case
    ErrorUnknown _ msg -> pretty msg


instance Errorable 'ErrorCode.LexBreakEncoding where
  data Error 'ErrorCode.LexBreakEncoding where
    -- FIXME: Take encoding kind and lexing error info
    ErrorLexBreakEncoding :: Error 'ErrorCode.LexBreakEncoding
    deriving (Eq, Show, Typeable)

  errorCode _ = ErrorCode.LexBreakEncoding
  detailedMessage _ = Nothing

instance Pretty (Error 'ErrorCode.LexBreakEncoding) where
  pretty e = pretty do show e -- FIXME


instance Errorable 'ErrorCode.LexUnclosedCommentBlock where
  data Error 'ErrorCode.LexUnclosedCommentBlock where
    -- FIXME: Take comment block kind and lexing error info
    ErrorUnclosedCommentBlock :: Error 'ErrorCode.LexUnclosedCommentBlock
    deriving (Eq, Show, Typeable)

  errorCode _ = ErrorCode.LexUnclosedCommentBlock
  detailedMessage _ = Nothing

instance Pretty (Error 'ErrorCode.LexUnclosedCommentBlock) where
  pretty e = pretty do show e -- FIXME
