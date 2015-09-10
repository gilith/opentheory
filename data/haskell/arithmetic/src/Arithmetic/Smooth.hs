{- |
module: Arithmetic.Smooth
description: Smooth numbers
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Arithmetic.Smooth
where

import qualified Data.List as List
import OpenTheory.Primitive.Natural
import qualified OpenTheory.Natural.Bits as Bits
import OpenTheory.Natural.Divides

import Arithmetic.Prime

factorOut :: Natural -> Natural -> Maybe (Natural,Natural)
factorOut p =
    go 0
  where
    go k n =
      if divides p n then go (k + 1) (n `div` p)
      else if k == 0 then Nothing
      else Just (k,n)

factorList :: [Natural] -> Natural -> ([(Natural,Natural)],Natural)
factorList ps n =
    case ps of
      [] -> ([],n)
      p : pt ->
        case factorOut p n of
	  Nothing -> factorList pt n
	  Just (k,m) ->
            let (pks,q) = factorList pt m in
            ((p,k) : pks, q)

factorBase :: Natural -> Natural -> ([(Natural,Natural)],Natural)
factorBase k = factorList (take (fromIntegral k) primes)

multiplyBase :: ([(Natural,Natural)],Natural) -> Natural
multiplyBase =
    \(pks,m) -> foldr mult m pks
  where
    mult (p,k) m = (p ^ k) * m

newtype Smooth =
    Smooth {unSmooth :: ([(Natural,Natural)],Natural)}
  deriving (Eq,Ord)

instance Show Smooth where
  show s =
      if null factors then "1" else List.intercalate "*" factors
    where
      factors = map showPk pks ++ showRest
      showRest = if n == 1 then [] else [showWidth]
      showWidth = if w < 20 then show n
                  else "[" ++ show w ++ "]"
      showPk (p,k) = show p ++ showExp k
      showExp k = if k == 1 then "" else "^" ++ show k
      (pks,n) = unSmooth s
      w = Bits.width n

fromNatural :: Natural -> Natural -> Smooth
fromNatural k = Smooth . factorBase k

toNatural :: Smooth -> Natural
toNatural = multiplyBase . unSmooth

factoring :: Smooth -> Maybe [(Natural,Natural)]
factoring s =
    if n == 1 then Just pks else Nothing
  where
    (pks,n) = unSmooth s

next :: Natural -> Natural -> Smooth
next k =
    go
  where
    go n =
        case factoring s of
          Nothing -> go (n + 1)
          Just _ -> s
      where
        s = fromNatural k n
