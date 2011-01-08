{-# LANGUAGE StandaloneDeriving #-}
{- |
Module: $Header$
Description: A verified UTF8 parser
License: MIT
License-file: LICENSE

Maintainer: Joe Hurd
-}
module Main(main) where

import Test.QuickCheck

import OpenTheory.Char

instance Arbitrary Plane where
  arbitrary =
      do x <- Test.QuickCheck.arbitrarySizedIntegral
         let x' = if x < 0 then -x else x
         let x'' = x' `mod` 17
         return (Plane x'')

instance Arbitrary Position where
  arbitrary = fmap Position arbitrary

instance Arbitrary Unicode where
  arbitrary = fmap (\(pl,pos) -> Unicode pl pos) arbitrary

checkArgs :: Test.QuickCheck.Args
checkArgs = Test.QuickCheck.stdArgs { maxSuccess = 100 }

check :: Testable prop => String -> prop -> IO ()
check name prop =
  do putStr (name ++ ": ")
     Test.QuickCheck.quickCheckWith checkArgs prop

prop1 :: [OpenTheory.Char.Unicode] -> Bool
prop1 cs =
    case OpenTheory.Char.decode (OpenTheory.Char.encode cs) of
      Just cs' -> cs == cs'
      Nothing -> False

main :: IO ()
main = do check "print then parse" prop1
