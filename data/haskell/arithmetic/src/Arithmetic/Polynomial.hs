{- |
module: Arithmetic.Polynomial
description: Polynomial arithmetic
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Arithmetic.Polynomial
where

import OpenTheory.Primitive.Natural
import OpenTheory.List

import Arithmetic.Utility
import qualified Arithmetic.Ring as Ring

data Polynomial a =
     Polynomial
       {ring :: Ring.Ring a,
        coefficients :: [a]}

zero :: Ring.Ring a -> Polynomial a
zero r =
    Polynomial
      {ring = r,
       coefficients = []}

isZero :: Polynomial a -> Bool
isZero p =
    case coefficients p of
      [] -> True
      _ -> False

constant :: Ring.Ring a -> a -> Polynomial a
constant r x =
    Polynomial
      {ring = r,
       coefficients = [x]}

isConstant :: Polynomial a -> Bool
isConstant p =
    case coefficients p of
      [] -> True
      [_] -> True
      _ -> False

degree :: Polynomial a -> Natural
degree = naturalLength . coefficients
