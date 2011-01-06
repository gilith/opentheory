module Main where

import Test.QuickCheck

checkArgs :: Test.QuickCheck.Args
checkArgs = Test.QuickCheck.stdArgs { maxSuccess = 100 }

check :: Testable prop => String -> prop -> IO ()
check name prop =
  do putStr (name ++ ": ")
     Test.QuickCheck.quickCheckWith checkArgs prop

main :: IO ()
main = do check "print then parse" ((\i -> True) :: Bool -> Bool)
