{- |
module: Arithmetic.SquareRoot
description: Natural number square root
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Arithmetic.SquareRoot
where

import OpenTheory.Primitive.Natural
import qualified Data.List as List

import qualified Arithmetic.ContinuedFraction as ContinuedFraction

floor :: Natural -> Natural
floor n =
    if n < 2 then n else bisect 0 n
  where
    bisect l u =
        if m == l then l
	else if m * m <= n then bisect m u
	else bisect l m
      where
        m = (l + u) `div` 2

ceiling :: Natural -> Natural
ceiling n =
    if sqrtn * sqrtn == n then sqrtn else sqrtn + 1
  where
    sqrtn = Arithmetic.SquareRoot.floor n

continuedFraction :: Natural -> ContinuedFraction.ContinuedFraction
continuedFraction n =
    ContinuedFraction.ContinuedFraction (sqrtn,qs)
  where
    sqrtn = Arithmetic.SquareRoot.floor n

    ps = continuedFractionPeriodicTail n sqrtn

    qs = if null ps then [] else cycle ps

continuedFractionPeriodic :: Natural -> [Natural]
continuedFractionPeriodic n =
    continuedFractionPeriodicTail n sqrtn
  where
    sqrtn = Arithmetic.SquareRoot.floor n

continuedFractionPeriodicTail :: Natural -> Natural -> [Natural]
continuedFractionPeriodicTail n sqrtn =
    List.unfoldr go (sqrtn,sqrtd)
  where
    sqrtd = n - sqrtn * sqrtn

-- (sqrt(n) + a) / b = c + 1 / x ==>
-- x = b / (sqrt(n) + a - c * b)
--   = b / (sqrt(n) - (c * b - a))
--   = (b * (sqrt(n) + (c * b - a))) / (n - (c * b - a)^2)
    advance (a,b) =
        (c,(d,e))
      where
        c = (sqrtn + a) `div` b
        d = c * b - a
        e = (n - d * d) `div` b

    go (a,b) =
        case b of
          0 -> Nothing
          1 -> Just (2 * a, (0,0))
          _ -> Just (advance (a,b))
