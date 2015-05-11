{- |
module: Egcd
description: A natural number implementation of the egcd algorithm
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Egcd
where

import OpenTheory.Primitive.Natural

integerEgcd :: Integer -> Integer -> (Integer,Integer,Integer)
integerEgcd a b =
    if b == 0 then (1,0,a) else
    let (s,t,g) = integerEgcd b (a `mod` b) in
    (t, s - (a `div` b) * t, g)

naturalEgcd :: Natural -> Natural -> (Natural,Natural,Natural)
naturalEgcd a b =
    if b == 0 then (1,0,a) else
    let c = a `mod` b in
    if c == 0 then (1, a `div` b - 1, b) else
    let (s,t,g) = naturalEgcd c (b `mod` c) in
    let u = s + (b `div` c) * t in
    (u, t + (a `div` b) * u, g)
