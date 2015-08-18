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
    ContinuedFraction {unContinuedFraction :: (Natural,[Natural])}
  deriving Eq

fromNatural :: Natural -> ContinuedFraction
fromNatural n = ContinuedFraction (n,[])

goldenRatio :: ContinuedFraction
goldenRatio = ContinuedFraction (1, repeat 1)

approximate :: (Natural -> a) -> (a -> a -> a) -> (a -> a) ->
               ContinuedFraction -> Natural -> a
approximate lift add inv =
    \ (ContinuedFraction (n,l)) -> go (lift n) l
  where
    go n l depth =
        if depth == 0 then n
        else case l of
               [] -> n
               h : t -> add n (inv (go (lift h) t (depth - 1)))

toFractional :: Fractional a => ContinuedFraction -> Natural -> a
toFractional =
    approximate fromIntegral (+) inv
  where
    inv x = 1.0 / x

toDouble :: ContinuedFraction -> Double
toDouble x = toFractional x 53

instance Show ContinuedFraction where
  show = show . toDouble

{-
fromRealFrac :: RealFrac a => a -> ContinuedFraction
fromRealFrac =
    ContinuedFraction . go
  where
    go x =
        n : (if y == 0.0 then [] else go (1.0 / y))
      where
        (n,y) = properFraction x
-}
