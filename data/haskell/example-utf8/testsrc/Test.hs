{- |
Module: $Header$
Description: A verified UTF8 parser
License: MIT
License-file: LICENSE

Maintainer: Joe Hurd
-}
module Main(main) where

import Data.Word
import OpenTheory.Char
import Test.QuickCheck

instance Arbitrary Plane where
  arbitrary = fmap Plane (Test.QuickCheck.suchThat arbitrary predicate)
      where
    predicate i = 0 <= i && i < 17

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

prop2 :: [Data.Word.Word8] -> Bool
prop2 bs =
    case OpenTheory.Char.decode bs of
      Just cs -> OpenTheory.Char.encode cs == bs
      Nothing -> True

prop3 :: [OpenTheory.Char.Unicode] -> Bool
prop3 cs = length cs <= length (OpenTheory.Char.encode cs)

main :: IO ()
main =
    do check "print then parse" prop1
       check "parse then print" prop2
       check "printing grows length" prop3
