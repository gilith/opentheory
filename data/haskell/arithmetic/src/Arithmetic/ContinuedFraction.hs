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

naturalLogarithmBase :: ContinuedFraction
naturalLogarithmBase =
    ContinuedFraction (2, go 2)
  where
    go n = 1 : n : 1 : go (n + 2)

convergentsFn :: (Natural -> a) -> (a -> a -> a) -> (a -> a -> a) ->
                 [Natural] -> a -> a -> [a]
convergentsFn lift add mult =
    go
  where
    go [] _ _ = []
    go (q : qs) y x =
        z : go qs z y
      where
        z = add (mult (lift q) y) x

numerators :: (Natural -> a) -> (a -> a -> a) -> (a -> a -> a) ->
              ContinuedFraction -> [a]
numerators lift add mult (ContinuedFraction (q0,qs)) =
    x : convergentsFn lift add mult qs x one
  where
    x = lift q0
    one = lift 1

denominators :: (Natural -> a) -> (a -> a -> a) -> (a -> a -> a) ->
                ContinuedFraction -> [a]
denominators lift add mult (ContinuedFraction (_,qs)) =
    one : convergentsFn lift add mult qs one zero
  where
    one = lift 1
    zero = lift 0

convergents :: (Natural -> a) -> (a -> a -> a) -> (a -> a -> a) ->
               (a -> a -> a) -> ContinuedFraction -> [a]
convergents lift add mult divf cf =
    zipWith divf nums dens
  where
    nums = numerators lift add mult cf
    dens = denominators lift add mult cf

unstableConvergents :: Eq a => [a] -> [a]
unstableConvergents [] = error "empty convergents"
unstableConvergents (q0 : qs) =
    q0 : go q0 qs
  where
    go _ [] = []
    go x (h : t) = if x == h then [] else h : go h t

fractionalConvergents :: Fractional a => ContinuedFraction -> [a]
fractionalConvergents = convergents fromIntegral (+) (*) (/)

rationalConvergents :: ContinuedFraction -> [Rational]
rationalConvergents = convergents fromIntegral (+) (*) (/)

toDouble :: ContinuedFraction -> Double
toDouble = last . unstableConvergents . fractionalConvergents

instance Show ContinuedFraction where
  show = show . toDouble

fromRealFrac :: RealFrac a => a -> ContinuedFraction
fromRealFrac x =
    ContinuedFraction (q0, go y)
  where
    go s =
      if s == 0.0 then []
      else let (q,t) = properFraction (1.0 / s) in q : go t

    (q0,y) = properFraction x

invert :: ContinuedFraction -> Maybe ContinuedFraction
invert (ContinuedFraction (q0,qs)) =
    if q0 /= 0 then Just (ContinuedFraction (0, q0 : qs))
    else
      case qs of
        [] -> Nothing
        h : t -> Just (ContinuedFraction (h,t))
