{- |
module: Arithmetic.ContinuedFraction
description: Continued fractions
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Arithmetic.ContinuedFraction
where

import OpenTheory.Primitive.Natural

newtype ContinuedFraction =
    ContinuedFraction {unContinuedFraction :: [Natural]}

fromRealFrac :: RealFrac a => a -> ContinuedFraction
fromRealFrac =
    ContinuedFraction . go
  where
    go x =
        n : (if y == 0.0 then [] else go (1.0 / y))
      where
        (n,y) = properFraction x
