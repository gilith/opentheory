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
       {carrier :: Ring.Ring a,
        coefficients :: [a]}

zero :: Ring.Ring a -> Polynomial a
zero r =
    Polynomial
      {carrier = r,
       coefficients = []}

isZero :: Polynomial a -> Bool
isZero p =
    case coefficients p of
      [] -> True
      _ -> False

constant :: Eq a => Ring.Ring a -> a -> Polynomial a
constant r x =
    if x == Ring.zero r then zero r
    else
      Polynomial
        {carrier = r,
         coefficients = [x]}

destConstant :: Polynomial a -> Maybe a
destConstant p =
    case coefficients p of
      [] -> Just (Ring.zero r)
      [c] -> Just c
      _ -> Nothing
  where
    r = carrier p

isConstant :: Polynomial a -> Bool
isConstant p =
    case destConstant p of
      Just _ -> True
      Nothing -> False

fromNatural :: Eq a => Ring.Ring a -> Natural -> Polynomial a
fromNatural r = constant r . Ring.fromNatural r

degree :: Polynomial a -> Natural
degree = naturalLength . coefficients

-- Horner's method
evaluate :: Polynomial a -> a -> a
evaluate p x =
    foldr eval (Ring.zero r) (coefficients p)
  where
    r = carrier p
    eval c z = Ring.add r c (Ring.multiply r x z)

add :: Eq a => Polynomial a -> Polynomial a -> Polynomial a
add p q =
    Polynomial
      {carrier = r,
       coefficients = addc (coefficients p) (coefficients q)}
  where
    r = carrier p
    z = Ring.zero r

    norm c [] = if c == z then [] else [c]
    norm c cs = c : cs

    addc [] ql = ql
    addc pl [] = pl
    addc (ph : pt) (qh : qt) = norm (Ring.add r ph qh) (addc pt qt)

negate :: Polynomial a -> Polynomial a
negate p =
    Polynomial
      {carrier = r,
       coefficients = map (Ring.negate r) pl}
  where
    r = carrier p
    pl = coefficients p

multiply :: Eq a => Polynomial a -> Polynomial a -> Polynomial a
multiply p q =
    case coefficients q of
      [] -> zero r
      qh : qt ->
          Polynomial
            {carrier = r,
             coefficients = foldr multc [] (coefficients p)}
        where
          multc pc cs =
              if pc == z then z : cs
              else Ring.multiply r pc qh : addc (map (Ring.multiply r pc) qt) cs
  where
    r = carrier p
    z = Ring.zero r

    addc xs [] = xs
    addc [] ys = ys
    addc (x : xs) (y : ys) = Ring.add r x y : addc xs ys

invert :: Polynomial a -> Maybe (Polynomial a)
invert p =
    case coefficients p of
      [x] -> case Ring.invert r x of
               Nothing -> Nothing
               Just y -> Just (Polynomial {carrier = r, coefficients = [y]})
      _ -> Nothing
  where
    r = carrier p

ring :: Eq a => Ring.Ring a -> Ring.Ring (Polynomial a)
ring r =
    Ring.Ring {Ring.fromNatural = fromNatural r,
               Ring.add = add,
               Ring.negate = Arithmetic.Polynomial.negate,
               Ring.multiply = multiply,
               Ring.invert = invert}
