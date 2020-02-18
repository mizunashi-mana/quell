module Language.Quell.Parse.Type where

import Language.Quell.Prelude



data ParseCtx (m :: Type -> Type) (a :: Type) = ParseCtx
  deriving (Functor)

instance Applicative m => Applicative (ParseCtx m) where
  pure = undefined
  mf <*> mx = undefined mf mx

instance Monad m => Monad (ParseCtx m) where
  m >>= f = undefined m f

instance MonadIO m => MonadIO (ParseCtx m) where
  liftIO = undefined


data Token
  -- special
  = TEof
  | TUnknown Text

  -- reserved
  | TLet
  | TWhere

  -- for layout
  | TOpenBrace
  | TCloseBrace
  | TVOpenBrace
  | TVCloseBrace
  | TSemiColon

  -- identifier
  | TVarId FastString

  -- literal
  | TChar Text Char

  -- for document
  | TComment Text
  deriving Show

deriving instance Eq Token
