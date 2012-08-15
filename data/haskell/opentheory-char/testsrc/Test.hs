{- |
module: Main
description: Unicode characters - testing
license: MIT

maintainer: Joe Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified OpenTheory.Data.Byte as Data.Byte
import qualified OpenTheory.Data.List as Data.List
import qualified OpenTheory.Data.List.Geometric as Data.List.Geometric
import qualified OpenTheory.Data.Option as Data.Option
import qualified OpenTheory.Data.Unicode as Data.Unicode
import qualified OpenTheory.Data.Unicode.UTF8 as Data.Unicode.UTF8
import qualified OpenTheory.Primitive.Random as Primitive.Random
import qualified OpenTheory.Primitive.Test as Primitive.Test

proposition0 :: Primitive.Random.Random -> Bool
proposition0 r =
  let (cs, _) = Data.List.Geometric.fromRandom Data.Unicode.fromRandom r in
  Data.List.size cs <= Data.List.size (Data.Unicode.UTF8.encode cs)

proposition1 :: Primitive.Random.Random -> Bool
proposition1 r =
  let (cs, _) = Data.List.Geometric.fromRandom Data.Unicode.fromRandom r in
  Data.Option.equal (Data.List.equal Data.Unicode.equal)
    (Data.Unicode.UTF8.decode (Data.Unicode.UTF8.encode cs)) (Just cs)

proposition2 :: Primitive.Random.Random -> Bool
proposition2 r =
  let (bs, _) = Data.List.Geometric.fromRandom Data.Byte.fromRandom r in
  case Data.Unicode.UTF8.decode bs of
    Nothing -> True
    Just cs -> Data.List.equal (==) (Data.Unicode.UTF8.encode cs) bs

proposition3 :: Primitive.Random.Random -> Bool
proposition3 r =
  let (c, _) = Data.Unicode.fromRandom r in
  let (pl, pos) = Data.Unicode.unUnicode c in
  let pli = Data.Unicode.unPlane pl in
  let posi = Data.Unicode.unPosition pos in
  not (pli == 0) || posi < 55296 || 57343 < posi && posi < 65534

main :: IO ()
main =
    do Primitive.Test.check "Proposition 0:\n  !r.\n    let (cs, r') <- H.Geometric.fromRandom H.fromRandom r in\n    H.size cs <= H.size (H.UTF8.encode cs)\n  " proposition0
       Primitive.Test.check "Proposition 1:\n  !r.\n    let (cs, r') <- H.Geometric.fromRandom H.fromRandom r in\n    H.equal (H.equal H.equal) (H.UTF8.decode (H.UTF8.encode cs)) (some cs)\n  " proposition1
       Primitive.Test.check "Proposition 2:\n  !r.\n    let (bs, r') <- H.Geometric.fromRandom H.fromRandom r in\n    case H.UTF8.decode bs of\n      none -> T\n    | some cs -> H.equal (=) (H.UTF8.encode cs) bs\n  " proposition2
       Primitive.Test.check "Proposition 3:\n  !r.\n    let (c, r') <- H.fromRandom r in\n    let (pl, pos) <- H.unUnicode c in\n    let pli <- H.unPlane pl in\n    let posi <- H.unPosition pos in\n    ~(pli = 0) \\/ posi < 55296 \\/ 57343 < posi /\\ posi < 65534\n  " proposition3
       return ()
