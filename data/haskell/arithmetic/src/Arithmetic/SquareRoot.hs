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
