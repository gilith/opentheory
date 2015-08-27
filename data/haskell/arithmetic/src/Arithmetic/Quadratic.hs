{- |
module: Arithmetic.Quadratic
description: Natural number square root
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Arithmetic.Quadratic
where

import OpenTheory.Primitive.Natural
import qualified Data.List as List

import qualified Arithmetic.ContinuedFraction as ContinuedFraction

rootFloor :: Natural -> Natural
rootFloor n =
    if n < 2 then n else bisect 0 n
  where
    bisect l u =
        if m == l then l
	else if m * m <= n then bisect m u
	else bisect l m
      where
        m = (l + u) `div` 2

rootCeiling :: Natural -> Natural
rootCeiling n =
    if sqrtn * sqrtn == n then sqrtn else sqrtn + 1
  where
    sqrtn = rootFloor n

rootContinuedFraction :: Natural -> ContinuedFraction.ContinuedFraction
rootContinuedFraction n =
    ContinuedFraction.ContinuedFraction (sqrtn,qs)
  where
    sqrtn = rootFloor n

    ps = rootContinuedFractionPeriodicTail n sqrtn

    qs = if null ps then [] else cycle ps

rootContinuedFractionPeriodic :: Natural -> [Natural]
rootContinuedFractionPeriodic n =
    rootContinuedFractionPeriodicTail n sqrtn
  where
    sqrtn = rootFloor n

rootContinuedFractionPeriodicTail :: Natural -> Natural -> [Natural]
rootContinuedFractionPeriodicTail n sqrtn =
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
