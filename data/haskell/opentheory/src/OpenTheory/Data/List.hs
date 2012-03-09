{- |
Module: $Header$
Description: List types
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable

List types
-}
module OpenTheory.Data.List
  ( equal,
    size )
where

import qualified OpenTheory.Number.Natural

equal :: (a -> a -> Bool) -> [a] -> [a] -> Bool
equal _ [] [] = True
equal _ [] (_ : _) = False
equal _ (_ : _) [] = False
equal eq (h1 : t1) (h2 : t2) = eq h1 h2 && equal eq t1 t2

size :: [a] -> OpenTheory.Number.Natural.Natural
size [] = 0
size (_ : l) = size l + 1
