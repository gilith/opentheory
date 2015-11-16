{- |
module: Arithmetic.Polynomial
description: Polynomial arithmetic
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Arithmetic.Polynomial
where

import OpenTheory.Primitive.Natural
import OpenTheory.List
import Data.List as List
import Data.Maybe as Maybe

import qualified Arithmetic.Ring as Ring

data Polynomial a =
     Polynomial
       {carrier :: Ring.Ring a,
        coefficients :: [a]}

instance (Eq a, Show a) => Show (Polynomial a) where
  show p =
      if null ps then "0"
      else List.intercalate " + " ps
    where
      r = carrier p
      z = Ring.zero r
      o = Ring.one r
      ps = showC (0 :: Natural) (coefficients p)

      showC _ [] = []
      showC k (x : xs) = showM x k ++ showC (k + 1) xs

      showM x k =
          if x == z then []
          else [(if k /= 0 && x == o then "" else show x) ++
                (if k == 0 then ""
                 else ("x" ++ (if k == 1 then "" else "^" ++ show k)))]

fromCoefficients :: Eq a => Ring.Ring a -> [a] -> Polynomial a
fromCoefficients r cs =
    Polynomial
      {carrier = r,
       coefficients = norm cs}
  where
    z = Ring.zero r

    zcons x xs = if null xs && x == z then [] else x : xs

    norm [] = []
    norm (x : xs) = zcons x (norm xs)

zero :: Ring.Ring a -> Polynomial a
zero r =
    Polynomial
      {carrier = r,
       coefficients = []}

isZero :: Polynomial a -> Bool
isZero = null . coefficients

constant :: Eq a => Ring.Ring a -> a -> Polynomial a
constant r x = fromCoefficients r [x]

destConstant :: Polynomial a -> Maybe a
destConstant p =
    case coefficients p of
      [] -> Just (Ring.zero r)
      [c] -> Just c
      _ -> Nothing
  where
    r = carrier p

isConstant :: Polynomial a -> Bool
isConstant = Maybe.isJust . destConstant

fromNatural :: Eq a => Ring.Ring a -> Natural -> Polynomial a
fromNatural r = constant r . Ring.fromNatural r

one :: Eq a => Ring.Ring a -> Polynomial a
one r = constant r (Ring.one r)

multiplyByPower :: Polynomial a -> Natural -> Polynomial a
multiplyByPower p k =
    if k == 0 || null cs then p
    else p {coefficients = replicate (fromIntegral k) z ++ cs}
  where
    r = carrier p
    z = Ring.zero r
    cs = coefficients p

monomial :: Eq a => Ring.Ring a -> a -> Natural -> Polynomial a
monomial r x = multiplyByPower (constant r x)

variablePower :: Eq a => Ring.Ring a -> Natural -> Polynomial a
variablePower r = monomial r (Ring.one r)

variable :: Eq a => Ring.Ring a -> Polynomial a
variable r = variablePower r 1

degree :: Polynomial a -> Natural
degree = naturalLength . coefficients

leadingCoefficient :: Polynomial a -> Maybe a
leadingCoefficient p =
    case coefficients p of
      [] -> Nothing
      cs -> Just (last cs)

nthCoefficient :: Polynomial a -> Natural -> a
nthCoefficient p k =
    if k < degree p then coefficients p !! (fromIntegral k)
    else Ring.zero (carrier p)

isMonic :: Eq a => Polynomial a -> Bool
isMonic p =
    case leadingCoefficient p of
      Nothing -> False
      Just c -> c == Ring.one (carrier p)

-- Horner's method
evaluate :: Polynomial a -> a -> a
evaluate p x =
    foldr eval (Ring.zero r) (coefficients p)
  where
    r = carrier p
    eval c z = Ring.add r c (Ring.multiply r x z)

addCoefficients :: Ring.Ring a -> [a] -> [a] -> [a]
addCoefficients r =
    addc
  where
    addc [] ys = ys
    addc xs [] = xs
    addc (x : xs) (y : ys) = Ring.add r x y : addc xs ys

add :: Eq a => Polynomial a -> Polynomial a -> Polynomial a
add p q =
    fromCoefficients r (addCoefficients r ps qs)
  where
    r = carrier p
    ps = coefficients p
    qs = coefficients q

negate :: Polynomial a -> Polynomial a
negate p =
    Polynomial
      {carrier = r,
       coefficients = map (Ring.negate r) pl}
  where
    r = carrier p
    pl = coefficients p

multiply :: Eq a => Polynomial a -> Polynomial a -> Polynomial a
multiply p q =
    case coefficients q of
      [] -> zero r
      qh : qt ->
          fromCoefficients r (foldr multc [] (coefficients p))
        where
          z = Ring.zero r

          madd pc cs = addCoefficients r (map (Ring.multiply r pc) qt) cs

          multc pc cs =
              if pc == z then z : cs
              else Ring.multiply r pc qh : madd pc cs
  where
    r = carrier p

multiplyByScalar :: Eq a => Polynomial a -> a -> Polynomial a
multiplyByScalar p x = multiply p (constant (carrier p) x)

invert :: Polynomial a -> Maybe (Polynomial a)
invert p =
    case coefficients p of
      [x] -> case Ring.invert r x of
               Nothing -> Nothing
               Just y -> Just (Polynomial {carrier = r, coefficients = [y]})
      _ -> Nothing
  where
    r = carrier p

subtract :: Eq a => Polynomial a -> Polynomial a -> Polynomial a
subtract p = Ring.subtract (ring (carrier p)) p

quotientRemainder :: Eq a => Polynomial a -> Polynomial a ->
                     Maybe (Polynomial a, Polynomial a)
quotientRemainder p q =
    if d_p < d_q then Just (zero r, p)
    else case leadingCoefficient q of
           Nothing -> Nothing
           Just q_m ->
               go [] p (d_p - d_q)
             where
               sub f k =
                   if f_m == z then Just (z,f)
                   else case Ring.divide r f_m q_m of
                          Nothing -> Nothing
                          Just c ->
                              Just (c, Arithmetic.Polynomial.subtract f g)
                            where
                              g = multiplyByPower (multiplyByScalar q c) k
                 where
                   f_m = nthCoefficient f (d_q + k)

               go cs f k =
                   case sub f k of
                     Nothing -> Nothing
                     Just (c,g) -> go' (c : cs) g k

               go' cs f k =
                   if k == 0 then Just (fromCoefficients r cs, f)
                   else go cs f (k - 1)
  where
    r = carrier p
    z = Ring.zero r
    d_p = degree p
    d_q = degree q

divide :: Eq a => Polynomial a -> Polynomial a -> Maybe (Polynomial a)
divide p q =
    case quotientRemainder p q of
      Nothing -> Nothing
      Just (x,y) -> if isZero y then Just x else Nothing

ring :: Eq a => Ring.Ring a -> Ring.Ring (Polynomial a)
ring r =
    Ring.Ring {Ring.fromNatural = fromNatural r,
               Ring.add = add,
               Ring.negate = Arithmetic.Polynomial.negate,
               Ring.multiply = multiply,
               Ring.divide = divide}
