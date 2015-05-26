{- |
module: $Header$
description: The divides relation on natural numbers
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module OpenTheory.Natural.Divides
where

import qualified OpenTheory.Primitive.Natural as Natural

egcd ::
  Natural.Natural -> Natural.Natural ->
    (Natural.Natural, (Natural.Natural, Natural.Natural))
egcd a b =
  if b == 0 then (a, (1, 0))
  else
    let c = a `mod` b in
    if c == 0 then (b, (1, a `div` b - 1))
    else
      let (g, (s, t)) = egcd c (b `mod` c) in
      let u = s + b `div` c * t in
      (g, (u, t + a `div` b * u))

chineseRemainder ::
  Natural.Natural -> Natural.Natural -> Natural.Natural ->
    Natural.Natural -> Natural.Natural
chineseRemainder a b =
  let (_, (s, t)) = egcd a b in
  let ab = a * b in
  let sa = s * a in
  let tb = (a - t) * b in
  \x y -> (x * tb + y * sa) `mod` ab

divides :: Natural.Natural -> Natural.Natural -> Bool
divides a b = if a == 0 then b == 0 else b `mod` a == 0
