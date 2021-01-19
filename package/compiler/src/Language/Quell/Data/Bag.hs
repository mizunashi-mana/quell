module Language.Quell.Data.Bag (
    T,
    Bag,
    fromList,
) where

import           Language.Lexer.Tlex.Prelude


type T = Bag

data Bag a
    = ListBag [a]
    | Bags [Bag a]
    deriving (Show, Functor)

instance Foldable Bag where
    foldr k z0 = \case
        ListBag xs  -> foldr k z0 xs
        Bags bs     -> foldr
            do \b z -> foldr k z b
            z0 bs

    foldMap f = \case
        ListBag xs  -> foldMap f xs
        Bags bs     -> foldMap
            do \b -> foldMap f b
            bs

instance Eq a => Eq (Bag a) where
    b1 == b2 = toList b1 == toList b2

instance Semigroup (Bag a) where
    ListBag []   <> b2           = b2
    Bags []      <> b2           = b2
    ListBag [x1] <> ListBag xs2  = ListBag do x1:xs2
    Bags [b1]    <> Bags bs2     = Bags do b1:bs2
    b1           <> ListBag []   = b1
    b1           <> Bags []      = b1
    b1           <> Bags bs2     = Bags do b1:bs2
    b1           <> b2           = Bags [b1, b2]

instance Monoid (Bag a) where
    mempty = ListBag []

instance Applicative Bag where
    pure x = ListBag [x]
    mf <*> mx = case mf of
        Bags mfs ->
            Bags [ mf1 <*> mx | mf1 <- mfs ]
        ListBag fs -> case mx of
            ListBag xs ->
                ListBag do fs <*> xs
            Bags mxs ->
                Bags [ fmap f mx1 | f <- fs, mx1 <- mxs ]

instance Monad Bag where
    mx >>= f = case mx of
        ListBag xs ->
            Bags [ f x | x <- xs ]
        Bags mxs ->
            Bags [ mx1 >>= f | mx1 <- mxs ]

fromList :: [a] -> Bag a
fromList xs = ListBag xs
