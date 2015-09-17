{- |
module: Arithmetic.Utility.Heap
description: Leftist heaps
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Arithmetic.Utility.Heap
  ( Heap,
    size,
    isEmpty,
    empty,
    add,
    remove,
    toList )
where

data Node a =
    E
  | T Int a (Node a) (Node a)
  deriving Show

data Heap a =
    Heap (a -> a -> Bool) Int (Node a)

singleton :: a -> Node a
singleton a = T 1 a E E

rank :: Node a -> Int
rank E = 0
rank (T r _ _ _) = r

mkT :: a -> Node a -> Node a -> Node a
mkT a x y =
    if rx <= ry
      then T (rx + 1) a y x
      else T (ry + 1) a x y
  where
    rx = rank x
    ry = rank y

merge :: (a -> a -> Bool) -> Node a -> Node a -> Node a
merge le =
    mrg
  where
    mrg n1 n2 =
      case n1 of
        E -> n2
        T _ a1 x1 y1 ->
          case n2 of
            E -> n1
            T _ a2 x2 y2 ->
              if le a1 a2
                then mkT a1 x1 (mrg y1 n2)
                else mkT a2 x2 (mrg n1 y2)

size :: Heap a -> Int
size (Heap _ k _) = k

isEmpty :: Heap a -> Bool
isEmpty h = size h == 0

empty :: (a -> a -> Bool) -> Heap a
empty le = Heap le 0 E

add :: a -> Heap a -> Heap a
add a (Heap le k n) = Heap le (k + 1) (merge le (singleton a) n)

remove :: Heap a -> Maybe (a, Heap a)
remove (Heap le k n) =
    case n of
      E -> Nothing
      T _ a x y -> Just (a, Heap le (k - 1) (merge le x y))

toList :: Heap a -> [a]
toList h =
    case remove h of
      Nothing -> []
      Just (a,h') -> a : toList h'

instance Show a => Show (Heap a) where
  show = show . toList
