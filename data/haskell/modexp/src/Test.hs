{- |
module: Main
description: Testing the modular exponentiation computation
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified Data.List as List
import qualified System.Random
import OpenTheory.Primitive.Natural
import qualified OpenTheory.Primitive.Random as Random
import qualified OpenTheory.Natural.Uniform as Uniform

import qualified Prime
import qualified ModExp
import qualified Montgomery

modExpFns :: [(String, Natural -> Natural -> Natural -> Natural)]
modExpFns =
    [("naive", ModExp.modExp),
     ("montgomery", Montgomery.modExp)]

modDoubleExpFns :: [(String, Natural -> Natural -> Natural -> Natural)]
modDoubleExpFns =
    [("naive", ModExp.modDoubleExp),
     ("montgomery", Montgomery.modDoubleExp)]

parameters :: Int -> Random.Random -> (Natural,Natural,Natural)
parameters w r =
    (n,x,k)
  where
    n = Prime.randomPrime w r1
    x = Uniform.random n r2
    k = Uniform.random n r3
    (r1,r23) = Random.split r
    (r2,r3) = Random.split r23

checkModExp :: Natural -> Natural -> Natural -> IO ()
checkModExp n x k =
    case res of
      [] -> error "no modExp functions defined"
      (_,y) : ys ->
        if all ((==) y . snd) ys
          then putStrLn $ "( " ++ show x ++ " ^ " ++ show k ++ " ) `mod` " ++
                          show n ++ " == " ++ show y
          else error $ "different results for ( " ++ show x ++ " ^ " ++
                       show k ++ " ) `mod` " ++ show n ++ ":\n  " ++
                       List.intercalate "\n  "
                         (map (\ (s,y) -> s ++ ": " ++ show y) res)
  where
    res = map (\ (s,f) -> (s, f n x k)) modExpFns

checkModDoubleExp :: Natural -> Natural -> Natural -> IO ()
checkModDoubleExp n x k =
    case res of
      [] -> error "no modDoubleExp functions defined"
      (_,y) : ys ->
        if all ((==) y . snd) ys
          then putStrLn $ "( " ++ show x ++ " ^ 2 ^ " ++ show k ++
                          " ) `mod` " ++ show n ++ " == " ++ show y
          else error $ "different results for ( " ++ show x ++ " 2 ^ ^ " ++
                       show k ++ " ) `mod` " ++ show n ++ ":\n  " ++
                       List.intercalate "\n  "
                         (map (\ (s,y) -> s ++ ": " ++ show y) res)
  where
    res = map (\ (s,f) -> (s, f n x k)) modDoubleExpFns

checkRandomModExp :: Int -> Random.Random -> IO Random.Random
checkRandomModExp w r =
    do checkModExp n x k
       return r2
  where
    (n,x,k) = parameters w r1
    (r1,r2) = Random.split r

checkRandomModDoubleExp :: Int -> Int -> Random.Random -> IO Random.Random
checkRandomModDoubleExp w k r =
    do checkModDoubleExp n x (fromIntegral k)
       return r2
  where
    (n,x,_) = parameters w r1
    (r1,r2) = Random.split r

main :: IO ()
main =
    do s <- System.Random.randomIO
       let w = 2000
       let k = 1000000
       let r0 = Random.fromInt s
       r1 <- checkRandomModExp w r0
       checkRandomModDoubleExp w k r1
       return ()
