{- |
module: $Header$
description: Fibonacci numbers
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}

module OpenTheory.Natural.Fibonacci
where

import qualified Data.List as List
import qualified OpenTheory.Primitive.Natural as Natural

decode :: [Bool] -> Natural.Natural
decode =
  dest 1 0
  where
  {-dest ::
        Natural.Natural -> Natural.Natural -> [Bool] -> Natural.Natural-}
    dest _ _ [] = 0
    dest f p (h : t) =
      let s = f + p in let n = dest s f t in if h then s + n else n

encode :: Natural.Natural -> [Bool]
encode =
  \n -> find n 1 0
  where
  {-find ::
        Natural.Natural -> Natural.Natural -> Natural.Natural -> [Bool]-}
    find n f p = let s = f + p in if n < s then mk [] n f p else find n s f

  {-mk ::
        [Bool] -> Natural.Natural -> Natural.Natural -> Natural.Natural ->
          [Bool]-}
    mk l n f p =
      if p == 0 then l
      else if f <= n then mk (True : l) (n - f) p (f - p)
      else mk (False : l) n p (f - p)

zeckendorf :: [Bool] -> Bool
zeckendorf [] = True
zeckendorf (h : t) =
  if List.null t then h else not (h && head t) && zeckendorf t
