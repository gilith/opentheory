{- |
module: Main
description: Testing packages exported from OpenTheory
license: MIT

maintainer: Joe Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Main
  ( main )
where

import qualified Data.ByteString.Lazy
import qualified Data.List
import qualified Data.Maybe
import qualified Data.Word
import qualified System.Directory

import qualified OpenTheory.Data.Unicode.UTF8
import qualified OpenTheory.Number.Natural.Prime

readTestCharFile :: Bool -> FilePath -> IO ()
readTestCharFile x f =
    do b <- Data.ByteString.Lazy.readFile ("test/char/" ++ f ++ ".txt")
       let bs = Data.ByteString.Lazy.unpack b
       let a = Data.Maybe.isJust (OpenTheory.Data.Unicode.UTF8.decode bs)
       if a == x
         then return ()
         else
           if a
             then error $ "invalid file " ++ f ++ " was successfully decoded"
             else error $ "valid file " ++ f ++ " could not be decoded"

readValidCharFile :: FilePath -> IO ()
readValidCharFile = readTestCharFile True

readInvalidCharFile :: FilePath -> IO ()
readInvalidCharFile = readTestCharFile False

partitionTestCharFile :: IO ()
partitionTestCharFile =
    do skip <- System.Directory.doesFileExist "test/char/valid/test0.txt"
       if skip
         then return ()
         else
           do putStrLn "\n(splitting UTF8 test file into valid and invalid lines)"
              bytestr <- Data.ByteString.Lazy.readFile "test/char/test.txt"
              let bs = Data.ByteString.Lazy.unpack bytestr
              let ls = Data.List.drop 51 (readLines [] bs)
              outputLine 0 0 ls
  where
    readLines :: [[Data.Word.Word8]] -> [Data.Word.Word8] -> [[Data.Word.Word8]]
    readLines acc [] = reverse acc
    readLines acc inp =
      let (line,inp') = Data.List.span (not . isNewline) inp in
      let inp'' = Data.List.dropWhile isNewline inp' in
      let acc' = if null line then acc else line : acc in
      readLines acc' inp''

    isNewline :: Data.Word.Word8 -> Bool
    isNewline b = b == 10 || b == 13

    outputLine :: Int -> Int -> [[Data.Word.Word8]] -> IO ()
    outputLine _ _ [] = return ()
    outputLine ex err (line : rest) =
       let res = OpenTheory.Data.Unicode.UTF8.decode line in
       let (f,ex',err') =
               case res of
                 Just cs ->
                     if length cs == 79
                       then ("valid/test" ++ show ex, ex + 1, err)
                       else error $ "bad line length: " ++ show (length cs)
                 Nothing -> ("invalid/test" ++ show err, ex, err + 1) in
       let bs = Data.ByteString.Lazy.pack line in
       do Data.ByteString.Lazy.writeFile ("test/char/" ++ f ++ ".txt") bs
          outputLine ex' err' rest

testInitialPrimes :: IO ()
testInitialPrimes =
    let b = take 20 OpenTheory.Number.Natural.Prime.all ==
              [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71] in
    if b then return () else error "wrong initial 20 primes"

main :: IO ()
main =
    do putStr "Testing opentheory-char... "
       partitionTestCharFile
       readValidCharFile "demo"
       mapM_ (\i -> readValidCharFile $ "valid/test" ++ show i)
         ([0..139] :: [Int])
       mapM_ (\i -> readInvalidCharFile $ "invalid/test" ++ show i)
         ([0..70] :: [Int])
       putStrLn "ok"
       putStr "Testing opentheory-prime... "
       testInitialPrimes
       putStrLn "ok"
       return ()
