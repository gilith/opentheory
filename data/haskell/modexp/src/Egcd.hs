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
    if b == 0 then (a,1,0) else
    let (g,s,t) = integerEgcd b (a `mod` b) in
    (g, t, s - (a `div` b) * t)

naturalEgcd :: Natural -> Natural -> (Natural,Natural,Natural)
naturalEgcd a b =
    if b == 0 then (a,1,0) else
    let c = a `mod` b in
    if c == 0 then (b, 1, a `div` b - 1) else
    let (g,s,t) = naturalEgcd c (b `mod` c) in
    let u = s + (b `div` c) * t in
    (g, u, t + (a `div` b) * u)
