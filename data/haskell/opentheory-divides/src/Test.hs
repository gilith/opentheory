{- |
module: Main
description: The divides relation on natural numbers - testing
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified OpenTheory.Natural as Natural
import qualified OpenTheory.Natural.Divides as Divides
import qualified OpenTheory.Primitive.Natural as Primitive.Natural
import OpenTheory.Primitive.Test

proposition0 :: Primitive.Natural.Natural -> Bool
proposition0 a = Divides.divides a 0

proposition1 :: Primitive.Natural.Natural -> Bool
proposition1 a = Divides.divides a a

proposition2 :: Primitive.Natural.Natural -> Bool
proposition2 a = Divides.divides 1 a

proposition3 ::
  Primitive.Natural.Natural -> Primitive.Natural.Natural -> Bool
proposition3 a b = Divides.divides (fst (Divides.egcd a b)) a

proposition4 ::
  Primitive.Natural.Natural -> Primitive.Natural.Natural -> Bool
proposition4 a b = Divides.divides (fst (Divides.egcd a b)) b

proposition5 :: Primitive.Natural.Natural -> Bool
proposition5 a = Divides.divides 2 a == Natural.naturalEven a

proposition6 ::
  Primitive.Natural.Natural -> Primitive.Natural.Natural -> Bool
proposition6 ap b =
  let a = ap + 1 in let (_, (_, t)) = Divides.egcd a b in t < a

proposition7 ::
  Primitive.Natural.Natural -> Primitive.Natural.Natural -> Bool
proposition7 ap b =
  let a = ap + 1 in let (_, (s, _)) = Divides.egcd a b in s < max b 2

proposition8 ::
  Primitive.Natural.Natural -> Primitive.Natural.Natural -> Bool
proposition8 ap b =
  let a = ap + 1 in
  let (g, (s, t)) = Divides.egcd a b in
  t * b + g == s * a

proposition9 ::
  Primitive.Natural.Natural -> Primitive.Natural.Natural ->
    Primitive.Natural.Natural -> Primitive.Natural.Natural -> Bool
proposition9 ap bp xp yp =
  let aq = ap + 1 in
  let bq = bp + 1 in
  let g = fst (Divides.egcd aq bq) in
  let a = aq `div` g in
  let b = bq `div` g in
  let x = xp `mod` a in
  let y = yp `mod` b in
  Divides.chineseRemainder a b x y < a * b

proposition10 ::
  Primitive.Natural.Natural -> Primitive.Natural.Natural ->
    Primitive.Natural.Natural -> Primitive.Natural.Natural -> Bool
proposition10 ap bp xp yp =
  let aq = ap + 1 in
  let bq = bp + 1 in
  let g = fst (Divides.egcd aq bq) in
  let a = aq `div` g in
  let b = bq `div` g in
  let x = xp `mod` a in
  let y = yp `mod` b in
  Divides.chineseRemainder a b x y `mod` a == x

proposition11 ::
  Primitive.Natural.Natural -> Primitive.Natural.Natural ->
    Primitive.Natural.Natural -> Primitive.Natural.Natural -> Bool
proposition11 ap bp xp yp =
  let aq = ap + 1 in
  let bq = bp + 1 in
  let g = fst (Divides.egcd aq bq) in
  let a = aq `div` g in
  let b = bq `div` g in
  let x = xp `mod` a in
  let y = yp `mod` b in
  Divides.chineseRemainder a b x y `mod` b == y

main :: IO ()
main =
    do check "Proposition 0:\n  !a. divides a 0\n  " proposition0
       check "Proposition 1:\n  !a. divides a a\n  " proposition1
       check "Proposition 2:\n  !a. divides 1 a\n  " proposition2
       check "Proposition 3:\n  !a b. divides (fst (egcd a b)) a\n  " proposition3
       check "Proposition 4:\n  !a b. divides (fst (egcd a b)) b\n  " proposition4
       check "Proposition 5:\n  !a. divides 2 a <=> even a\n  " proposition5
       check "Proposition 6:\n  !ap b. let a <- ap + 1 in let (g, s, t) <- egcd a b in t < a\n  " proposition6
       check "Proposition 7:\n  !ap b. let a <- ap + 1 in let (g, s, t) <- egcd a b in s < max b 2\n  " proposition7
       check "Proposition 8:\n  !ap b. let a <- ap + 1 in let (g, s, t) <- egcd a b in t * b + g = s * a\n  " proposition8
       check "Proposition 9:\n  !ap bp xp yp.\n    let aq <- ap + 1 in\n    let bq <- bp + 1 in\n    let g <- fst (egcd aq bq) in\n    let a <- aq div g in\n    let b <- bq div g in\n    let x <- xp mod a in\n    let y <- yp mod b in\n    chineseRemainder a b x y < a * b\n  " proposition9
       check "Proposition 10:\n  !ap bp xp yp.\n    let aq <- ap + 1 in\n    let bq <- bp + 1 in\n    let g <- fst (egcd aq bq) in\n    let a <- aq div g in\n    let b <- bq div g in\n    let x <- xp mod a in\n    let y <- yp mod b in\n    chineseRemainder a b x y mod a = x\n  " proposition10
       check "Proposition 11:\n  !ap bp xp yp.\n    let aq <- ap + 1 in\n    let bq <- bp + 1 in\n    let g <- fst (egcd aq bq) in\n    let a <- aq div g in\n    let b <- bq div g in\n    let x <- xp mod a in\n    let y <- yp mod b in\n    chineseRemainder a b x y mod b = y\n  " proposition11
       return ()
