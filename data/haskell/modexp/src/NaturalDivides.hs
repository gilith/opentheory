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
divides a b = if a == 0 then b == 0 else b `mod` a == 0

egcd :: Natural -> Natural -> (Natural,(Natural,Natural))
egcd a b =
    if b == 0 then (a,(1,0)) else
    let c = a `mod` b in
    if c == 0 then (b, (1, a `div` b - 1)) else
    let (g,(s,t)) = egcd c (b `mod` c) in
    let u = s + (b `div` c) * t in
    (g, (u, t + (a `div` b) * u))

chineseRemainder :: Natural -> Natural -> Natural -> Natural -> Natural
chineseRemainder a b =
    \x y -> (x * tb + y * sa) `mod` ab
  where
    (_,(s,t)) = egcd a b
    ab = a * b
    sa = s * a
    tb = (a - t) * b
