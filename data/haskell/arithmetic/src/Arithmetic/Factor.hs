{- |
module: Arithmetic.Factor
description: Factorized natural numbers
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Arithmetic.Factor
where

import qualified Data.List as List
import qualified Data.Map as Map
import OpenTheory.Primitive.Natural

import Arithmetic.Utility

newtype Factor = Factor {unFactor :: Map.Map Natural Natural}

one :: Factor
one = Factor {unFactor = Map.empty}

isOne :: Factor -> Bool
isOne = Map.null . unFactor

primePower :: Natural -> Natural -> Factor
primePower p k = if k == 0 then one else Factor {unFactor = Map.singleton p k}

prime :: Natural -> Factor
prime p = primePower p 1

multiply :: Factor -> Factor -> Factor
multiply f1 f2 =
    Factor {unFactor = Map.unionWith (+) m1 m2}
  where
    m1 = unFactor f1
    m2 = unFactor f2

gcd :: Factor -> Factor -> Factor
gcd f1 f2 =
    Factor {unFactor = Map.intersectionWith min m1 m2}
  where
    m1 = unFactor f1
    m2 = unFactor f2

trialDivision :: [Natural] -> Natural -> (Factor,Natural)
trialDivision =
    go
  where
    go [] n = (one,n)
    go (p : ps) n =
        if n <= 1 then (one,n)
        else (multiply f (primePower p r), m)
      where
        (r,s) = factorOut p n
        (f,m) = go ps s

multiplicative :: (Natural -> Natural -> a) -> (a -> a -> a) -> a -> Factor -> a
multiplicative pkA multA oneA f =
    case Map.foldrWithKey inc Nothing (unFactor f) of
      Nothing -> oneA
      Just x -> x
  where
    inc p k acc = mult (pkA p k) acc
    mult x Nothing = Just x
    mult x (Just y) = Just (multA x y)

toNatural :: Factor -> Natural
toNatural = multiplicative (^) (*) 1

totient :: Factor -> Natural
totient =
    multiplicative tot (*) 1
  where
    tot p k = (p ^ (k - 1)) * (p - 1)

instance Show Factor where
  show =
      multiplicative showPK (++) "1"
    where
      showPK p k = show p ++ showExp k
      showExp k = if k == 1 then "" else "^" ++ show k
