{- |
Module: Main
Description: Unicode characters - testing
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable
-}
module Main
  ( main )
where

import qualified OpenTheory.Data.Unicode as Data.Unicode
import qualified OpenTheory.Primitive.Random as Primitive.Random
import qualified OpenTheory.Primitive.Test as Primitive.Test

proposition0 :: Primitive.Random.Random -> Bool
proposition0 r =
  let (c, _) = Data.Unicode.fromRandom r in
  let (pl, pos) = Data.Unicode.unUnicode c in
  let pli = Data.Unicode.unPlane pl in
  let posi = Data.Unicode.unPosition pos in
  not (pli == 0) || posi < 55296 || 57343 < posi && posi < 65534

main :: IO ()
main =
    do Primitive.Test.check "Proposition 0:\n  !r.\n    let (c, r') <- H.fromRandom r in\n    let (pl, pos) <- H.unUnicode c in\n    let pli <- H.unPlane pl in\n    let posi <- H.unPosition pos in\n    ~(pli = 0) \\/ posi < 55296 \\/ 57343 < posi /\\ posi < 65534\n  " proposition0
       return ()
