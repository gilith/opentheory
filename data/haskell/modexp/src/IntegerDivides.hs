{- |
module: IntegerDivides
description: Integer division algorithms
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module IntegerDivides
where

divides :: Integer -> Integer -> Bool
divides 0 b = b == 0
divides a b = abs b `mod` abs a == 0

egcd :: Integer -> Integer -> (Integer,(Integer,Integer))
egcd a 0 = (a,(1,0))
egcd a b =
    (g, (t, s - (a `div` b) * t))
  where
    (g,(s,t)) = egcd b (a `mod` b)

chineseRemainder :: Integer -> Integer -> Integer -> Integer -> Integer
chineseRemainder a b =
    \x y -> (x * tb + y * sa) `mod` ab
  where
    (_,(s,t)) = egcd a b
    ab = a * b
    sa = s * a
    tb = t * b
