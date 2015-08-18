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

floorSqrt :: Natural -> Natural
floorSqrt n =
    if n < 2 then n else bisect 0 n
  where
    bisect l u =
        if m == l then l
	else if m * m <= n then bisect m u
	else bisect l m
      where
        m = (l + u) `div` 2

continuedFractionSqrt :: Natural -> ContinuedFraction.ContinuedFraction
continuedFractionSqrt n =
    ContinuedFraction.ContinuedFraction (sqrtn,qs)
  where
    sqrtn = floorSqrt n

    ps = continuedFractionPeriodic n sqrtn

    qs = if null ps then [] else cycle ps

continuedFractionPeriodicSqrt :: Natural -> [Natural]
continuedFractionPeriodicSqrt n = continuedFractionPeriodic n (floorSqrt n)

continuedFractionPeriodic :: Natural -> Natural -> [Natural]
continuedFractionPeriodic n sqrtn =
    List.unfoldr go (sqrtn,init)
  where
    init = n - sqrtn * sqrtn

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
