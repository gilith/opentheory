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
import qualified Data.Maybe as Maybe

import Arithmetic.Utility
import qualified Arithmetic.ContinuedFraction as ContinuedFraction
import qualified Arithmetic.Modular as Modular

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

destSquare :: Natural -> Maybe Natural
destSquare n =
    if sqrtn * sqrtn == n then Just sqrtn else Nothing
  where
    sqrtn = rootFloor n

isSquare :: Natural -> Bool
isSquare = Maybe.isJust . destSquare

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

data Residue = Residue | NonResidue | Zero
    deriving (Eq,Ord,Show)

-- The first argument (the modulus) must be an odd natural
jacobiSymbol :: Natural -> Natural -> Residue
jacobiSymbol =
    \n -> if n == 1 then const Residue else go False n
  where
    go f n m =
        if p == 0 then Zero
        else if s == 1 then if g then NonResidue else Residue
        else go h s n
      where
        p = m `mod` n
        (r,s) = factorTwos p
        n8 = n `mod` 8
        n8_17 = n8 == 1 || n8 == 7
        n4_1 = n8 == 1 || n8 == 5
        s4_1 = s `mod` 4 == 1
        g = if even r || n8_17 then f else not f
        h = if n4_1 || s4_1 then g else not g

-- The first argument (the modulus) must be an odd natural greater than 1
isResidue :: Natural -> Natural -> Bool
isResidue n m =
    case jacobiSymbol n m of
      Residue -> True
      _ -> False

-- The first argument (the modulus) must be an odd natural greater than 1
isNonResidue :: Natural -> Natural -> Bool
isNonResidue n m =
    case jacobiSymbol n m of
      NonResidue -> True
      _ -> False

-- The first argument (the modulus) must be an odd natural greater than 1
nextResidue :: Natural -> Natural -> Natural
nextResidue n =
    loop
  where
    loop m = if isResidue n m then m else loop (m + 1)

-- The first argument (the modulus) must be an odd natural greater than 1
nextNonResidue :: Natural -> Natural -> Natural
nextNonResidue n =
    loop
  where
    loop m = if isNonResidue n m then m else loop (m + 1)

-- The first argument (the modulus) must be a prime congruent to 3 mod 4
-- The second argument must be a residue modulo the prime
rootModuloPrime3Mod4 :: Natural -> Natural -> Natural
rootModuloPrime3Mod4 p =
    \n -> Modular.exp p n k
  where
    k = (p + 1) `div` 4

-- The first argument (the modulus) must be a prime congruent to 5 mod 8
-- The second argument must be a residue modulo the prime
rootModuloPrime5Mod8 :: Natural -> Natural -> Natural
rootModuloPrime5Mod8 p =
    go
  where
    go n =
        Modular.multiply p n (Modular.multiply p v (i - 1))
      where
        m = Modular.double p n
        v = Modular.exp p m k
        i = Modular.multiply p m (Modular.square p v)

    k = (p - 5) `div` 8

-- The Tonelli-Shanks algorithm
-- The first argument (the modulus) must be a prime
-- The second argument must be a residue modulo the prime
rootModuloPrime :: Natural -> Natural -> Natural
rootModuloPrime p =
    if p == 2 then Modular.normalize p
    else if r == 1 then rootModuloPrime3Mod4 p
    else if r == 2 then rootModuloPrime5Mod8 p
    else tonelliShanks
  where
    (r,s) = factorTwos (p - 1)
    z = Modular.exp p (nextNonResidue p 2) s

    tonelliShanks n =
        tonelliShanksLoop z d t r
      where
        d = Modular.exp p n ((s + 1) `div` 2)
        t = Modular.exp p n s

    tonelliShanksLoop c d t m =
        if t == 1 then d else tonelliShanksLoop b2 db tb2 i
      where
        i = tonelliShanksMin t 1
        b = Modular.exp2 p c (m - (i + 1))
        b2 = Modular.square p b
        db = Modular.multiply p d b
        tb2 = Modular.multiply p t b2

    tonelliShanksMin t i =
        if t2 == 1 then i else tonelliShanksMin t2 (i + 1)
      where
        t2 = Modular.square p t
