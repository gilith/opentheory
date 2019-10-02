{- |
module: Arithmetic.Pell
description: The Pell equation a^2 = n*b^2 + 1
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Arithmetic.Pell
where

import OpenTheory.Primitive.Natural

import Arithmetic.Utility
import qualified Arithmetic.Modular as Modular
import qualified Arithmetic.Quadratic as Quadratic

-------------------------------------------------------------------------------
-- Using the Chakravala method to find the fundamental solution of
-- the Pell equation
--
--   a^2 = n*b^2 + 1
--
-- (where n is not a square).
-------------------------------------------------------------------------------

chakravala :: Natural -> [(Natural,Natural,Natural)]
chakravala n =
    if sqrtN * sqrtN == n then []
    else let a = minM 0 1 in reduce a 1
  where
    reduce a b = (a,b,k) : (if k == 1 && a2 > nb2 then [] else reduce a' b')
      where
        a2 = a * a
        nb2 = n * b * b
        k =  distance a2 nb2
        a' = (a * m + n * b) `div` k
        b' = (a + b * m) `div` k
        j = case Modular.divide k (Modular.negate k a) b of
              Just i -> i
              Nothing -> error "Pell.chakravala: couldn't divide"
        m = minM j k

    sqrtN = Quadratic.rootFloor n

    minM j k =
        if n - m_0 * m_0 <= m_1 * m_1 - n then m_0 else m_1
      where
        m_0 = sqrtN - Modular.subtract k sqrtN j
        m_1 = m_0 + k

-------------------------------------------------------------------------------
-- Finding all integer solutions of the Pell equation
-------------------------------------------------------------------------------

solutions :: Natural -> [(Natural,Natural)]
solutions n =
    (a0,b0) : (if null l then [] else go a1 b1)
  where
    a0 = 1
    b0 = 0
    l = chakravala n
    (a1,b1,_) = last l
    go a b = (a,b) : go (a1 * a + n * b1 * b) (a1 * b + b1 * a)

solution :: Natural -> (Natural,Natural)
solution n =
    case solutions n of
      _ : ab : _ -> ab
      _ -> error "The Pell equation a^2 = n*b^2 + 1 has no nontrivial integer solution when n is square"
