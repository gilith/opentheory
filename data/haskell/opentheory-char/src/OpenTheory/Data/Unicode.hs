{- |
module: $Header$
description: Unicode characters
license: MIT

maintainer: Joe Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Data.Unicode
where

import qualified OpenTheory.Data.Word16 as Data.Word16
import qualified OpenTheory.Number.Natural.Uniform
  as Number.Natural.Uniform
import qualified OpenTheory.Primitive.Byte as Primitive.Byte
import qualified OpenTheory.Primitive.Random as Primitive.Random
import qualified OpenTheory.Primitive.Word16 as Primitive.Word16

newtype Plane = Plane { unPlane :: Primitive.Byte.Byte }

newtype Position = Position { unPosition :: Primitive.Word16.Word16 }

newtype Unicode = Unicode { unUnicode :: (Plane, Position) }

equalPlane :: Plane -> Plane -> Bool
equalPlane p1 p2 = unPlane p1 == unPlane p2

equalPosition :: Position -> Position -> Bool
equalPosition p1 p2 = unPosition p1 == unPosition p2

equal :: Unicode -> Unicode -> Bool
equal c1 c2 =
  let (pl1, pos1) = unUnicode c1 in
  let (pl2, pos2) = unUnicode c2 in
  equalPlane pl1 pl2 && equalPosition pos1 pos2

planeFromRandom ::
  Primitive.Random.Random -> (Plane, Primitive.Random.Random)
planeFromRandom r =
  let (n, r') = Number.Natural.Uniform.fromRandom 17 r in
  (Plane (Primitive.Byte.fromNatural n), r')

positionFromRandom ::
  Primitive.Random.Random -> (Position, Primitive.Random.Random)
positionFromRandom r =
  let (w, r') = Data.Word16.fromRandom r in (Position w, r')

fromRandom :: Primitive.Random.Random -> (Unicode, Primitive.Random.Random)
fromRandom r =
  let (pl, r') = planeFromRandom r in
  let pli = unPlane pl in
  let (pos, r'') =
        if not (pli == 0) then positionFromRandom r'
        else
          let (n, r''') = Number.Natural.Uniform.fromRandom 63486 r' in
          let n' = if n < 55296 then n else n + 2048 in
          (Position (Primitive.Word16.fromNatural n'), r''') in
  (Unicode (pl, pos), r'')
