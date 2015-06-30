{- |
module: NaturalDivides
description: Natural number division algorithms
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module NaturalDivides
where

import OpenTheory.Primitive.Natural

divides :: Natural -> Natural -> Bool
divides 0 b = b == 0
divides a b = b `mod` a == 0

egcd :: Natural -> Natural -> (Natural,(Natural,Natural))
egcd a 0 = (a,(1,0))
egcd a b =
    if c == 0
    then (b, (1, a `div` b - 1))
    else (g, (u, t + (a `div` b) * u))
  where
    c = a `mod` b
    (g,(s,t)) = egcd c (b `mod` c)
    u = s + (b `div` c) * t

chineseRemainder :: Natural -> Natural -> Natural -> Natural -> Natural
chineseRemainder a b =
    \x y -> (x * tb + y * sa) `mod` ab
  where
    (_,(s,t)) = egcd a b
    ab = a * b
    sa = s * a
    tb = (a - t) * b
