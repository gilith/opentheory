{- |
module: Divides
description: Natural number division algorithms
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Divides
where

import OpenTheory.Primitive.Natural

integerDivides :: Integer -> Integer -> Bool
integerDivides a b = if a == 0 then b == 0 else abs b `mod` abs a == 0

integerEgcd :: Integer -> Integer -> (Integer,Integer,Integer)
integerEgcd a b =
    if b == 0 then (a,1,0) else
    let (g,s,t) = integerEgcd b (a `mod` b) in
    (g, t, s - (a `div` b) * t)

naturalDivides :: Natural -> Natural -> Bool
naturalDivides a b = if a == 0 then b == 0 else b `mod` a == 0

naturalEgcd :: Natural -> Natural -> (Natural,Natural,Natural)
naturalEgcd a b =
    if b == 0 then (a,1,0) else
    let c = a `mod` b in
    if c == 0 then (b, 1, a `div` b - 1) else
    let (g,s,t) = naturalEgcd c (b `mod` c) in
    let u = s + (b `div` c) * t in
    (g, u, t + (a `div` b) * u)

integerChineseRemainder :: Integer -> Integer -> Integer -> Integer -> Integer
integerChineseRemainder a b =
    \x y -> (x * tb + y * sa) `mod` ab
  where
    (_,s,t) = integerEgcd a b
    ab = a * b
    sa = s * a
    tb = t * b

naturalChineseRemainder :: Natural -> Natural -> Natural -> Natural -> Natural
naturalChineseRemainder a b =
    \x y -> (x * tb + y * sa) `mod` ab
  where
    (_,s,t) = naturalEgcd a b
    ab = a * b
    sa = s * a
    tb = (a - t) * b
