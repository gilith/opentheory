{- |
Module: $Header$
Description: The Haskell base
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable
-}
module OpenTheory.Data.Option
where


equal :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
equal _ Nothing Nothing = True
equal _ Nothing (Just _) = False
equal _ (Just _) Nothing = False
equal eq (Just x1) (Just x2) = eq x1 x2
