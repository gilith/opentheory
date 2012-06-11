{- |
Module: $Header$
Description: Option types
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable

Option types
-}
module OpenTheory.Prelude.Data.Option
  ( equal )
where

equal :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
equal _ Nothing Nothing = True
equal _ Nothing (Just _) = False
equal _ (Just _) Nothing = False
equal eq (Just a1) (Just a2) = eq a1 a2
