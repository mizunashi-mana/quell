Module Semantics
================

Signatures
----------

::

  signature EqDict a where
    (==): a -> a -> Bool

  signature MonoListLike where
    type Container: Type
    type Element: Type

    map: (Element -> Element) -> Container -> Container
    length: Container -> Int

Modules and Functors
--------------------

::

  BoolList: MonoListLike
  module BoolList where
    data Container where
      BoolNil: Container
      BoolCons: Bool -> Container -> Container

    type Element = Bool

    map f xs0 = go xs0 where
      go = \
        BoolNil       -> BoolNil
        BoolCons x xs -> BoolCons
          do f x
          do go xs

    length = \
      BoolNil       -> 0
      BoolCons _ xs -> 1 + length xs

  data List a where
    Nil: List a
    Cons: a -> List a -> List a

  ListEqDict: EqDict a -> EqDict (List a)
  module ListEqDict eq where
    xs0 == ys0 = go xs0 ys0 where
      go = \
        Nil,         Nil         -> True
        Cons x' xs', Cons y' ys' -> x' eq.== y' && go xs' ys'
        _,           _           -> False

Traits
------

::

  {-!
  | ::
  |
  |   signature Eq a where
  |     (==): a -> a -> Bool
  |
  |-}
  trait Eq a where
    (==): a -> a -> Bool

  impl default: Eq (List a) <= Eq a where
    xs == ys = case xs, ys of
      Nil,         Nil         -> True
      Cons x' xs', Cons y' ys' -> x' == y' && xs' == ys'
      _,           _           -> False

Automatic Derive
----------------


