{- |
module: Arithmetic.Ring
description: An abstract ring type
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Arithmetic.Ring
where

import OpenTheory.Primitive.Natural
import qualified Data.Maybe as Maybe

import Arithmetic.Utility

data Ring a = Ring
    {fromNatural :: Natural -> a,
     add :: a -> a -> a,
     negate :: a -> a,
     multiply :: a -> a -> a,
     divide :: a -> a -> Maybe a}

zero :: Ring a -> a
zero r = fromNatural r 0

one :: Ring a -> a
one r = fromNatural r 1

two :: Ring a -> a
two r = fromNatural r 2

double :: Ring a -> a -> a
double r x = add r x x

subtract :: Ring a -> a -> a -> a
subtract r x y = add r x (Arithmetic.Ring.negate r y)

square :: Ring a -> a -> a
square r x = multiply r x x

exp :: Ring a -> a -> Natural -> a
exp r = multiplyExponential (multiply r) (one r)

exp2 :: Ring a -> a -> Natural -> a
exp2 r x k = functionPower (square r) k x

divides :: Ring a -> a -> a -> Bool
divides r x y = Maybe.isJust (divide r x y)

invert :: Ring a -> a -> Maybe a
invert r x = divide r (one r) x
