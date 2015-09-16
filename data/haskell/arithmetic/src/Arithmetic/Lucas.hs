{- |
module: Arithmetic.Lucas
description: Lucas sequences
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Arithmetic.Lucas
where

import OpenTheory.Primitive.Natural
import qualified OpenTheory.Natural.Bits as Bits

import Arithmetic.Utility

advance :: (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> a -> a
advance sub mult p q x y = sub (mult p y) (mult q x)

sequence :: (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> a -> [a]
sequence sub mult p q =
    go
  where
    go x y = x : go y (advance sub mult p q x y)

uSequence :: a -> a -> (a -> a -> a) -> (a -> a -> a) -> a -> a -> [a]
uSequence zero one sub mult p q =
    Arithmetic.Lucas.sequence sub mult p q zero one

vSequence :: a -> (a -> a -> a) -> (a -> a -> a) -> a -> a -> [a]
vSequence two sub mult p q =
    Arithmetic.Lucas.sequence sub mult p q two p
